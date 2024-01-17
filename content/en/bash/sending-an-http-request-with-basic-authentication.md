---
title:                "Sending an http request with basic authentication"
html_title:           "Bash recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? 
Sending an HTTP request with basic authentication involves including a user's credentials in the request header in order to access a protected resource on a server. Programmers use this method to ensure secure communication between a client and server, as the user's credentials are encoded in the request and then decoded by the server.

## How to:
To send an HTTP request with basic authentication in Bash, you can use the `curl` command with the `-u` flag followed by the username and password. For example:
```Bash
curl -u username:password https://example.com/protected-resource
```
This will send a GET request to the specified URL with the user's credentials in the request header. The server will then authenticate the user and grant access to the protected resource if the credentials are valid.

## Deep Dive:
- Historical context: Basic authentication is one of the oldest forms of user authentication on the internet and was first introduced in the early 1990s. It is still widely used today, although there are more secure alternatives available.
- Alternatives: As mentioned, there are more secure methods of authentication available such as Digest authentication or OAuth. These methods use additional encryption and hashing techniques to protect user credentials.
- Implementation details: Basic authentication works by encoding the username and password in base64 format and then including them in the `Authorization` header of the HTTP request. Once decoded by the server, the credentials are compared to a user database for authentication.

## See Also:
- [HTTP Basic authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [cURL man page](https://linux.die.net/man/1/curl)
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)