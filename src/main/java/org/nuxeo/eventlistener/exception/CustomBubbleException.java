package org.nuxeo.eventlistener.exception;

import org.nuxeo.ecm.core.api.RecoverableClientException;

public class CustomBubbleException extends RecoverableClientException {

    private static final long serialVersionUID = 1L;

    public CustomBubbleException(String message, String localizedMessage, String[] params) {
        super(message, localizedMessage, params);
    }

    public CustomBubbleException(String message, String localizedMessage, String[] params, Throwable cause) {
        super(message, localizedMessage, params, cause);
    }

}
