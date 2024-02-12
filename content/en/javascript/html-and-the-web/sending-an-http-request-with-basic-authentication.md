---
title:                "Sending an HTTP request with basic authentication"
aliases:
- /en/javascript/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:57.938512-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves including a username and password encoded in base64 within the request header. Programmers do this to access resources that need some form of simple validation to ensure some level of security.

## How to:

Here's a quick example using JavaScript's Fetch API:

```javascript
const url = 'https://some-protected-resource.com/data';
const username = 'YourUsername';
const password = 'YourPassword';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Network response was not ok.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fetch error: ', error));
```

Sample output (printed to the console):

```javascript
{
  "protected": "data",
  "moreData": 12345
}
```

## Deep Dive

Before diving in, let's get a bit of context. Basic authentication is one of the simplest forms of web service security, sending credentials in headers with every request.

Historical Context:
- Basic HTTP auth is an old method, initially outlined in the RFC 7617 from 2015, replacing the even older RFC 2617 from 1999.
- It was widely used due to its simplicity but isn't as secure without HTTPS, as base64 encoding is easily reversible.

Alternatives:
- OAuth: A more secure and complex standard for access delegation, used in cases where you need to provide access without sharing password credentials.
- API Keys: A single token that's easier to manage than complex OAuth protocols.
- Bearer Tokens: Particularly JWT (JSON Web Tokens), which can carry more information.

Implementation Details:
- Base64 encoding transforms the username:password string into a sequence of characters that's more universally transmittable.
- Always ensure the connection is HTTPS, to prevent credentials from being intercepted.
- Modern development favors tokens and session cookies for authentication, as they're more secure and versatile.

## See Also

- [Mozilla Developer Network - Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP Basic Auth](https://tools.ietf.org/html/rfc7617)
- [Introduction to OAuth 2.0](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [JSON Web Tokens (JWT)](https://jwt.io/introduction/)
