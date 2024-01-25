---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-01-20T18:01:23.805540-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves transmitting a username and password over the web to access protected resources. Programmers use this for simplicity when interacting with APIs or services requiring login creds.

## How to:

In Fish Shell, use `curl` to make an HTTP request with basic auth. Replace `username`, `password`, and `the_url`:

```Fish Shell
set -x AUTH (echo -n "username:password" | base64)
curl -H "Authorization: Basic $AUTH" the_url
```

Or, let `curl` handle the encoding:

```Fish Shell
curl -u username:password the_url
```

Sample output might look like this:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
{
  "message": "Authenticated successfully."
}
```

## Deep Dive

Basic authentication is part of HTTP's protocol, existing since the early 90s. While easy to implement, it's less secure due to credentials being only base64 encoded, not encrypted. HTTPS helps, but it's not foolproof.

Alternatives include OAuth, which uses tokens instead of credentials, adding security layers. For added security, consider using API keys or JWT (JSON Web Tokens).

With Fish Shell, we're interfacing with `curl`, a powerful tool that supports various protocols and authentication methods. The `-u` flag is convenient, but avoid hardcoding creds; instead, use environment variables or config files with proper permissions.

## See Also:

- cURL Documentation: https://curl.se/docs/httpscripting.html
- HTTP Basic Auth RFC: https://tools.ietf.org/html/rfc7617
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- Understanding JWT: https://jwt.io/introduction/