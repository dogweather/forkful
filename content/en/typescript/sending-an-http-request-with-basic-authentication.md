---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Sending HTTP Request with Basic Authentication in TypeScript 

## What & Why?
Sending an HTTP request with basic authentication is a simple way of managing access to a network resource. It essentially involves providing a username and password with our requests. It’s widely used because of its simplicity and – even if imperfect– adequate security for standard applications.

## How To:
When using the Fetch API to send HTTP requests in TypeScript, we can include basic auth data in headers.

```TypeScript
import fetch from 'node-fetch';

const url = 'https://example.com';
const username = 'username';
const password = 'password';

fetch(url, {
  headers: {
    'Authorization': 'Basic ' + Buffer.from(username + ':' + password).toString('base64')
  }
}).then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```
In the output, you'll see the data yielded by your request, or an error message.

## Deep Dive
HTTP Basic Authentication dates back to the early stages of the web. It isn’t the most secure measure due to lack of encryption of the communicated credentials, hence it's recommended to use it over HTTPS.

In terms of alternatives, we've things like Digest, NTLM, or OAuth. Each with different trade-offs. With OAuth, for instance, you use tokens instead of sending credentials, offering superior security but with increased complexity.

In Basic auth, client sends Base64 encoded credentials in `Authorization` header field as `Authorization: Basic base64credentials`. Browsers typically provide a pop-up dialog for the username and password and will remember them for the duration of the session.

## See Also
Take a look at these related resources:

1. Learn more about Fetch API via [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API).
2. Read about alternatives to basic auth in this [OAuth vs Basic Authentication article](https://dzone.com/articles/oauth-vs-basic-authentication-everything-you-wante).
3. Understand the [in-depth details of Basic access authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication).