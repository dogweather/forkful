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

Sending an HTTP request with basic authentication means including a username and password with your request so that the server can verify your identity. Programmers often do this to access protected resources or APIs, as it provides a simple and secure way to ensure that only authorized users have access.

## How to:

Fish Shell makes it easy to send an HTTP request with basic authentication using the curl command. Simply include the username and password in the URL, separated by a colon. Here's an example:

```
curl -u username:password http://example.com/protected-resource
```

This will send a GET request with the specified username and password, and the server will respond accordingly.

You can also use the -i flag to see the full response headers, or the -X flag to specify a different HTTP method. For more advanced usage, check out the documentation for curl and its various flags and options.

## Deep Dive

Basic authentication has been a part of the Hypertext Transfer Protocol (HTTP) since its early days in 1995. It is still widely used and supported, but has some security concerns and is not recommended for use in sensitive applications. In those cases, alternative methods such as OAuth or token-based authentication may be more appropriate.

When sending an HTTP request with basic authentication, the username and password are encoded using Base64, which is a simple encoding scheme that can be easily decoded by anyone who intercepts the request. This is one of the reasons why basic authentication is not suitable for sensitive data.

In Fish Shell, the -u flag is used to specify basic authentication, with the username and password separated by a colon. This is a quick and convenient way to include authentication with your HTTP requests, but it may not offer as much flexibility as other methods.

## See Also

- [Official Fish Shell documentation for curl](https://fishshell.com/docs/current/cmds/curl.html)
- [Explanation of basic authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Using OAuth for API authentication](https://oauth.net/2/)