---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Bash HTTP Requests With Basic Auth

## What & Why?

Sending an HTTP request with basic authentication involves including credentials within the request sent by Bash script. Programmers do this to access password-protected resources.

## How to:

Here's how you do it with the `curl` command.

```Bash
user="<your_username>"
password="<your_password>"
url="<api_url>"

# Send GET request
curl -u $user:$password $url
```

Upon running this command, you'll either get a response body or an error message.

## Deep Dive

Basic HTTP authentication is a simple solution that can solve lots of problems, but it's not always the perfect one. It was officially specified by the HTTP/1.0 release (RFC 1945) as a standard authorization method.

Although its implementation is straightforward, Basic Auth comes with a few security concerns. The main issue is that it merely Base64 encodes credentials, which does not provide any strong defense against eavesdroppers and is not recommended in a productive environment without the use of an SSL/TLS connection.

There are other methods like Digest authentication, OAuth, or JWT tokens that provide more secure ways of authorizing/verifying users or clients. Some applications might benefit more from these modern, secure, and flexible methods.

## See Also 

* GitHub's cURL Tutorial: https://guides.github.com/activities/hello-world/
* HTTP/1.0 Specification: https://www.w3.org/Protocols/HTTP/1.0/spec.html
* Wikipedia's Explanation on Basic Authentication: https://en.wikipedia.org/wiki/Basic_access_authentication
* Base64 Encoding: https://datatracker.ietf.org/doc/html/rfc4648
* Alternative Auth methods:
  - Digest authentication: https://datatracker.ietf.org/doc/html/rfc7616
  - OAuth: https://oauth.net/
  - JWT tokens: https://jwt.io/introduction