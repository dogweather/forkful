---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Programmers send HTTP requests with basic authentication to access protected data from a server. It's a simple way to limit access to certain data over the internet.

## How To:

Supply username and password with `-u` flag using `curl` command in Fish Shell.

```Fish Shell
set url "http://your.site.com/protected"
set username "your_username"
set password "your_password"

curl -u $username:$password $url
```
This will respond with the accessed content or an error message in case of failure.

## Deep Dive:

The use of basic authentication via HTTP requests is as old as the internet itself. It's a built-in feature of the HTTP protocol that allows a client (you) to supply a username/password pair to access server-side protected content.

Alternatively, you can opt for other forms like Digest Authentication, OAuth, OpenID, etc. based on your security requirements. Basic Authentication doesn't encrypt your credentials, so using it over HTTPS is recommended.

In the Fish Shell example above, your credentials are stored in plain text variables before passing to the `curl` command. Do note, sensitive data like passwords should be handled with utmost care, consider storing them in environment variables or other secure ways if you're dealing with real-world projects.

## See Also:

1. Fish Shell Documentation (https://fishshell.com/docs/current/index.html)
2. cURL man page (https://curl.se/docs/manpage.html)
3. HTTP Authentication (https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
4. Digest Authentication (https://tools.ietf.org/html/rfc2617)
5. OAuth 2.0 (https://oauth.net/2/)
6. OpenID Connect (https://openid.net/connect/)