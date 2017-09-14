package org.nuxeo.eventlistener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.event.DocumentEventTypes;
import org.nuxeo.ecm.core.event.Event;
import org.nuxeo.ecm.core.event.impl.DocumentEventContext;

/**
 * Prevents the modification of a document of type 'SUPNXP-21143' when fields of 
 * schema 'supnxp-21143_drc' are about to be updated.
 *
 * @see <a href="https://jira.nuxeo.com/browse/SUPNXP-21143">SUPNXP-21143</a>
 */
public class AbortDocumentModificationOnSchemaListener extends AbortDocumentCreationListener {

    protected static final String DOC_TYPE = "SUPNXP-21143";

    protected static final String SCHEMA_NAME = "supnxp-21143_drc";
    
    protected static final String ERROR_MESSAGE_PREFIX = "Document modification aborted: ";

    private static final Log LOGGER = LogFactory.getLog(AbortDocumentModificationOnSchemaListener.class);

    @Override
    protected boolean acceptedEvent(Event event) {
        return (DocumentEventTypes.BEFORE_DOC_UPDATE.equals(event.getName()));
    }

    /**
     * Returns <code>false</code> if schema <code>supnxp-21143_drc</code>'s fields changed, otherwise true.
     * If <code>false</code> is returned, document modification will be cancelled by throwing an exception.
     */
    @Override
    protected boolean documentComplies(DocumentEventContext ctx) {
        DocumentModel doc = ctx.getSourceDocument();
        // ignore documents of type other than 'SUPNXP-21143'
        if (!DOC_TYPE.equals(doc.getType())) {
            return true;
        }
        LOGGER.debug("<documentComplies> " + doc.getPathAsString());
        setMessage(ERROR_MESSAGE_PREFIX + "fields of schema '" + SCHEMA_NAME + "' are being updated");
        // returns 'false'  in order to cancel the modification of the document
        return !doc.getDataModel(SCHEMA_NAME).isDirty();
    }

}
