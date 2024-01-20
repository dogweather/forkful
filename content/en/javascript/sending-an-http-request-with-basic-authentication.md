---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication means passing credentials (username and password) in an HTTP header. Programmers do this to secure their API resources by providing an authentication layer over the connection.

## How To:
In JavaScript, a common way to send HTTP requests with basic authentication is by using `fetch`. Here's how you do it:

```Javascript
const url = 'https://api.mywebsite.com/data';
const username = 'example';
const password = '12345';

fetch(url, {
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + btoa(username + ":" + password)
  }
})
.then(response => response.json())
.then(json => console.log(json));
```
This will output the JSON response in the console.

## Deep Dive
Historically, Basic Authentication originated from the early web, before the development of visually interactive sites. Crosstalk between servers and browsers required a simple security scheme for credential verification.

Performance-wise, HTTP Basic Authentication might not be ideal for all use cases. Each request sends the username and password. Overheads increase as traffic grows. 

Alternatives include OAuth, token-based authentication, and JSON Web Tokens (JWT), which can provide more flexibility, improved security, and more efficient traffic flow.

For implementation details, note that the `fetch` method doesn't support CORS (Cross-Origin Resource Sharing) preflight. The `btoa` function encodes the username and password in base64 format. The header configuration `'Authorization': 'Basic ' + btoa(username+ ":" + password)` adds credentials to the HTTP request appropriately.

## See Also
- [Mozilla Developer Network (MDN) - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [MDN - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [MDN - Basic authentication scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Wikipedia - Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [How To Code -JWT vs Cookie](https://www.howtocode.io/learning-path/html-and-javascript-for-beginners/jwt-vs-cookie)
- [Token-Based Authentication With AngularJS & NodeJS](https://devdactic.com/jwt-authentication-ionic-node/)