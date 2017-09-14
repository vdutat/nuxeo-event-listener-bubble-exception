package org.nuxeo.eventlistener;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.event.CoreEventConstants;
import org.nuxeo.ecm.core.event.Event;
import org.nuxeo.ecm.core.event.impl.DocumentEventContext;
import org.nuxeo.runtime.api.Framework;
import org.nuxeo.runtime.services.config.ConfigurationService;


/**
 *
 */
public class AbortMoveToFolderEventListener extends AbortDocumentCreationListener {

    protected static final String PROPERTY_NAME = "nuxeo.bundle.nuxeo-event-listener.bubble-exception.config.folder";

	private static final Log LOG = LogFactory.getLog(AbortMoveToFolderEventListener.class);

    @Override
	protected boolean acceptedEvent(Event event) {
		return true;
	}

    @Override
	protected boolean documentComplies(DocumentEventContext ctx) {
        String destinationPath = (String) ctx.getProperty(CoreEventConstants.DESTINATION_PATH);
        if (StringUtils.isEmpty(destinationPath)) {
            return true;
        }
        ConfigurationService service = Framework.getService(ConfigurationService.class);
        String configFolder = service.getProperty(PROPERTY_NAME);
        if (!StringUtils.isEmpty(configFolder) && destinationPath.equals(configFolder)) {
            DocumentModel doc = ctx.getSourceDocument();
            setMessage(ERROR_MESSAGE_PREFIX + "move document aborted: " + doc.getPathAsString() + " to " + destinationPath);
            return false;
        }
        return true;
	}

}
