---
title:                "Sending an http request with basic authentication"
html_title:           "Java recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
HTTP requests with basic authentication are often used in web development to securely transmit user credentials, such as login information or API keys, to a server. This allows for secure access to restricted resources and helps protect sensitive data.

## How To
Sending an HTTP request with basic authentication in Java is a straightforward process. First, we need to import the necessary classes from the `java.net` package:
```Java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import java.net.HttpURLConnection;
```

Next, we need to create an instance of the `Authenticator` class and override the `getPasswordAuthentication()` method to provide the credentials for the request:
```Java
String username = "johnDoe";
String password = "securePassword";

Authenticator.setDefault(new Authenticator() {
    protected PasswordAuthentication getPasswordAuthentication() {
        return new PasswordAuthentication(username, password.toCharArray());
    }
});
```

Then, we can create a `URL` object with the URL we want to send the request to and open a connection to it using `HttpURLConnection`:
```Java
URL url = new URL("https://example.com/api/resource");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
```

We also need to set the request method to `GET` and add the `Authorization` header with the basic authentication credentials:
```Java
connection.setRequestMethod("GET");
connection.setRequestProperty("Authorization", "Basic " + java.util.Base64.getEncoder().encodeToString((username + ":" + password).getBytes()));
```

Finally, we can get the response code and message using `connection.getResponseCode()` and `connection.getResponseMessage()`, respectively:
```Java
int responseCode = connection.getResponseCode();
String responseMessage = connection.getResponseMessage();
```

## Deep Dive
Sending an HTTP request with basic authentication involves adding a `Authorization` header to the request with the user's credentials encoded in Base64 format. This ensures that the credentials are not sent in plain text and adds a layer of security to the request. It's important to note that basic authentication is not considered a secure method of authentication as the credentials can potentially be intercepted and decoded. Therefore, it's recommended to use more secure methods, such as OAuth, for transmitting sensitive data over the internet.

## See Also
- [HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [java.net.Authenticator](https://docs.oracle.com/javase/7/docs/api/java/net/Authenticator.html)
- [java.net.HttpURLConnection](https://docs.oracle.com/javase/7/docs/api/java/net/HttpURLConnection.html)
- [java.util.Base64](https://docs.oracle.com/javase/7/docs/api/java/util/Base64.html)