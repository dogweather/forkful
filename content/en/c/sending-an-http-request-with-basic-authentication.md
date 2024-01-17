---
title:                "Sending an http request with basic authentication"
html_title:           "C recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves appending a username and password to the HTTP request header to access protected resources on a server. Programmers use this method to ensure secure communication between the client and server and restrict access to sensitive information.

## How to:

To send an HTTP request with basic authentication in C, use the libcurl library. First, initialize the library with the `curl_global_init()` function. Then, set the username and password using `curl_easy_setopt()` and specify the authentication method with `CURLAUTH_BASIC`. Finally, make the request using `curl_easy_perform()`. Here's an example:

```
// Initialize libcurl
curl_global_init(CURL_GLOBAL_ALL);

// Set up request
CURL *curl = curl_easy_init();
curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);

// Make request
curl_easy_perform(curl);
```

The output will be the response from the server, which may contain the requested information or an error message.

## Deep Dive

Basic authentication is one of the oldest forms of web authentication, developed in the early days of the World Wide Web. It sends credentials in plaintext, which means they can be intercepted and read by anyone. Due to this security vulnerability, it is now considered a less secure method of authentication compared to other methods like OAuth.

An alternative to basic authentication is using OAuth, which allows for more secure communication between the client and server. However, basic authentication is still widely used, especially for simple and internal applications.

The implementation of basic authentication involves the client encoding the username and password in a specific format and adding it to the request header. The server then decodes the credentials and verifies them before granting access to the requested resource.

## See Also

- [libcurl](https://curl.haxx.se/libcurl/)
- [OAuth](https://oauth.net/)
- [HTTP Basic Authentication](https://tools.ietf.org/html/rfc7617)