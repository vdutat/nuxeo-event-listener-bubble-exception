package org.nuxeo.eventlistener;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.core.api.ClientException;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.RecoverableClientException;
import org.nuxeo.ecm.core.api.event.CoreEventConstants;
import org.nuxeo.ecm.core.event.DeletedDocumentModel;
import org.nuxeo.ecm.core.event.Event;
import org.nuxeo.ecm.core.event.EventListener;
import org.nuxeo.ecm.core.event.impl.DocumentEventContext;
import org.nuxeo.ecm.platform.web.common.exceptionhandling.ExceptionHelper;
import org.nuxeo.runtime.api.Framework;


/**
 * 
 */
public class AbortMoveToFolderEventListener implements EventListener {

    private static final Log LOG = LogFactory.getLog(AbortMoveToFolderEventListener.class);

    public void handleEvent(Event event) throws ClientException {

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
        String destinationPath = (String) event.getContext().getProperty(CoreEventConstants.DESTINATION_PATH);
        if (StringUtils.isEmpty(destinationPath)) {
            return;
        }
        String configFolder = (String) Framework.getProperty("nuxeo.bundle.nuxeo-event-listener.bubble-exception.config.folder");
        if (!StringUtils.isEmpty(configFolder) && destinationPath.equals(configFolder)) {
            LOG.warn("Move document aborted: " + doc.getPathAsString() + " to " + destinationPath);
            String message = "move aborted";
            event.markBubbleException();
            event.markRollBack();
            throw new ClientException(ExceptionHelper.unwrapException(new RecoverableClientException("Bubbling exception by " + AbortMoveToFolderEventListener.class.getName(), message, null)));
        }
    }    

}
