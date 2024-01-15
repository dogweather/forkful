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

## Why
Sending an HTTP request with basic authentication allows you to securely access protected resources on a server, such as a website or API. This type of authentication is commonly used by websites and applications to verify the identity of a user before granting them access to sensitive information.

## How To
To send an HTTP request with basic authentication, you will need to use two command-line tools: curl and base64. First, you will need to encode your username and password in base64 format, using the following command:
```Bash
encoded_credentials=$(echo -n "username:password" | base64)
```
Make sure to replace "username" and "password" with your actual credentials. Next, you can use the encoded credentials in the Authorization header of your request, using the -u flag in curl:
```Bash
curl -u $encoded_credentials https://example.com/api
```
This will include the encoded credentials in the request, allowing you to access the protected resource. 

## Deep Dive
Basic authentication, also known as Basic Access Authentication, is a simple method of authentication used by HTTP and other protocols. It works by sending the username and password in clear text, encoded in base64 format, in the Authorization header of the request. Despite its name, basic authentication is not very secure as the credentials can be easily decoded, making it important to only use it with HTTPS connections.

## See Also
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [curl documentation](https://curl.se/docs/manpage.html)
- [base64 documentation](https://linux.die.net/man/1/base64)