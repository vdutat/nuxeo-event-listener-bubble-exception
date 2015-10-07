package org.nuxeo.eventlistener.exception;

import org.nuxeo.ecm.core.api.ClientException;

public class CustomClientException extends ClientException {

    private static final long serialVersionUID = 1L;

    public CustomClientException(String message) {
        super(message);
    }
}
