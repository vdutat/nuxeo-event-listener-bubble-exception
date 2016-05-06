/*
 * (C) Copyright 2016 Nuxeo SA (http://nuxeo.com/) and others.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Contributors:
 *     vdutat
 */

package org.nuxeo.eventlistener;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.DocumentRef;
import org.nuxeo.ecm.core.api.IterableQueryResult;
import org.nuxeo.ecm.core.api.NuxeoException;
import org.nuxeo.ecm.core.api.RecoverableClientException;
import org.nuxeo.ecm.core.api.event.CoreEventConstants;
import org.nuxeo.ecm.core.api.event.DocumentEventTypes;
import org.nuxeo.ecm.core.event.DeletedDocumentModel;
import org.nuxeo.ecm.core.event.Event;
import org.nuxeo.ecm.core.event.EventListener;
import org.nuxeo.ecm.core.event.impl.DocumentEventContext;
import org.nuxeo.ecm.core.query.sql.NXQL;
import org.nuxeo.ecm.core.query.sql.model.Operator;
import org.nuxeo.ecm.platform.web.common.exceptionhandling.ExceptionHelper;
import org.nuxeo.eventlistener.exception.CustomClientException;

/**
 * Prevents the creation of a document of type 'File' when a document with the same title
 * in the same folder already exists.
 *
 * @see <a href="https://jira.nuxeo.com/browse/SUPNXP-14732">SUPNXP-14732</a>
 */
public class AbortCreateDocumentSameTitleEventListener implements EventListener {

    private static final Log LOG = LogFactory.getLog(AbortCreateDocumentSameTitleEventListener.class);

    static final List<String> docTypesToCheck = Arrays.asList("File");

    @Override
    public void handleEvent(Event event) {
        boolean restApiOnly = false;
        if (!DocumentEventTypes.ABOUT_TO_CREATE.equals(event.getName())) {
            return;
        }
        if (!(event.getContext() instanceof DocumentEventContext)) {
            return;
        }
        DocumentEventContext ctx = (DocumentEventContext) event.getContext();
        DocumentModel doc = ctx.getSourceDocument();
        // Skip shallow document
        if (doc instanceof DeletedDocumentModel) {
            return;
        }
        if (doc.isProxy() || doc.isVersion()) {
            return;
        }
        if (!documentComplies(ctx)) {
            LOG.warn(doc.getPathAsString() + " does not comply");
            String message = "document with same title already exists in " + (String) ctx.getProperty(CoreEventConstants.DESTINATION_PATH);
            event.markBubbleException();
            event.markRollBack();
            if (restApiOnly) {
                throw new CustomClientException(message); // only work through REST API, this gives an error page in JSF UI
            } else {
                throw new NuxeoException(ExceptionHelper.unwrapException(new RecoverableClientException("Bubbling exception by " + AbortCreateDocumentSameTitleEventListener.class.getName(), message, null)));
            }
        }
    }

    /**
     * @param ctx
     * @return <code>true</code> if document can be created, <code>false</code> to cancel document's creation
     */
    protected boolean documentComplies(DocumentEventContext ctx) {
        DocumentModel doc = ctx.getSourceDocument();
        String title = (String) doc.getPropertyValue("dc:title");
        // checking if document has a title
        if (StringUtils.isEmpty(title)) {
            // Ignore document
            return true;
        }
        // checking if document is created under a specific folderish document
        if (!StringUtils.startsWith((String) ctx.getProperty(CoreEventConstants.DESTINATION_PATH), "/default-domain/workspaces/ws1")) {
            // Ignore document
            return true;
        }
        // rejecting documents with same title only of type 'File'
        if (!docTypesToCheck.contains(doc.getType())) {
            // Ignore document
            return true;
        }
        // retrieving parent folderish document
        DocumentRef parentRef = (DocumentRef) ctx.getProperty(CoreEventConstants.DESTINATION_REF);
        DocumentModel parentDoc = ctx.getCoreSession().getDocument(parentRef);
        // searching for documents with same title in the parent folderish document
        StringBuilder sb = new StringBuilder("SELECT " + NXQL.ECM_UUID + " FROM Document WHERE ");
        sb.append("dc:title").append(Operator.EQ.toString()).append(NXQL.escapeString(title))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_PARENTID).append(Operator.EQ.toString()).append(NXQL.escapeString(parentDoc.getId()))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_ISCHECKEDIN).append(Operator.EQ.toString() + "0");
        LOG.debug("NXQL query: " + sb.toString());
        IterableQueryResult result = ctx.getCoreSession().queryAndFetch(sb.toString(), NXQL.NXQL);
        LOG.debug("result nbr: " + result.size());
        return (result.size() == 0);
    }
}
