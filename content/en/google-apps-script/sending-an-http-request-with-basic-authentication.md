---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-02-01T13:42:05.450260-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sending an HTTP request with basic authentication"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves attaching a username and a password to a request for access to a protected resource on a server. Programmers do this to securely access APIs or web services that require authentication.

## How to:

In Google Apps Script, you can send an HTTP GET request with basic authentication using the `UrlFetchApp` service. Here's a straightforward way to do it:

```Javascript
function sendAuthenticatedRequest() {
  var url = 'https://example.com/api/data'; // Change this to the API endpoint you're interested in
  var username = 'yourUsername'; // Replace with your actual username
  var password = 'yourPassword'; // Replace with your actual password
  var headers = {
    "Authorization": "Basic " + Utilities.base64Encode(username + ':' + password)
  };
  
  var options = {
    "method": "get",
    "headers": headers
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

This script constructs a basic authorization header by encoding the username and password in base64 and then attaches it to the HTTP request. When you run `sendAuthenticatedRequest()`, it sends a GET request to the specified URL with the appropriate headers for basic authentication. If all goes well, the server responds with the requested data, which the script logs.

## Deep Dive

Historically, basic authentication was a straightforward method for an HTTP user agent to provide a username and password when making a request. However, because it sends the username and password in base64 encoded form (which is easily decoded), it's not considered secure unless used in conjunction with TLS (HTTPS) to encrypt the credentials.

In Google Apps Script, using `Utilities.base64Encode()` facilitates the creation of the `Authorization` header but remember, without HTTPS, your credentials are vulnerably sent over the internet.

While basic auth is convenient for simple scripts and accessing some legacy systems, modern security practices prefer more secure methods like OAuth for authentication. Google Apps Script itself offers built-in OAuth support for accessing Google APIs and many third-party services. Nonetheless, understanding basic authentication is useful, especially when dealing with APIs that haven't moved to token-based authentication schemes or for quick, internal tools running in a secure environment.
