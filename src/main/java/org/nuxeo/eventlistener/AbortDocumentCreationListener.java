package org.nuxeo.eventlistener;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.NuxeoException;
import org.nuxeo.ecm.core.api.RecoverableClientException;
import org.nuxeo.ecm.core.api.event.DocumentEventTypes;
import org.nuxeo.ecm.core.event.DeletedDocumentModel;
import org.nuxeo.ecm.core.event.Event;
import org.nuxeo.ecm.core.event.EventListener;
import org.nuxeo.ecm.core.event.impl.DocumentEventContext;
import org.nuxeo.ecm.platform.web.common.exceptionhandling.ExceptionHelper;

public abstract class AbortDocumentCreationListener implements EventListener {

    protected static final String ERROR_MESSAGE_PREFIX = "Document creation aborted: ";

    private static final Log LOG = LogFactory.getLog(AbortDocumentCreationListener.class);

    protected String message = ERROR_MESSAGE_PREFIX + "unknown reason";

	@Override
	public void handleEvent(Event event) {
        if (!acceptedEvent(event)) {
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
        	abort(event);
        }
    }

	protected boolean acceptedEvent(Event event) {
		return (DocumentEventTypes.ABOUT_TO_CREATE.equals(event.getName()));
	}

	/**
	 * Returns <code>false</code> if document creation needs to be aborted, otherwise <code>true</code>.
	 * @param ctx Event context
	 * @return
	 */
	protected abstract boolean documentComplies(DocumentEventContext ctx);

	protected String getMessage() {
		return ERROR_MESSAGE_PREFIX + message;
	}

	protected void setMessage(String message) {
		this.message = message;
	}

	protected void abort(Event event) {
        DocumentEventContext ctx = (DocumentEventContext) event.getContext();
        DocumentModel doc = ctx.getSourceDocument();
        LOG.warn(doc.getPathAsString() + " does not comply");
        event.markBubbleException();
        event.markRollBack();
        throw new NuxeoException(ExceptionHelper.unwrapException(new RecoverableClientException("Bubbling exception by " + AbortDocumentCreationListener.class.getName(), getMessage(), null)));
	}
}
