---
title:                "Sending an http request with basic authentication"
html_title:           "Javascript recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a method commonly used by programmers to secure their web applications or APIs. It allows for a username and password to be sent along with the request, providing a basic level of authorization and access control. This is particularly useful for restricting access to sensitive data or functionality.

## How to:

To make an HTTP request with basic authentication in Javascript, you will need to use the built-in `fetch()` function, along with some additional options and headers. Here is an example of how to make a GET request to an API endpoint with basic authentication:

```Javascript
const url = 'https://exampleapi.com/user/profile';

fetch(url, {
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + btoa('username:password')
  }
})
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```

In this example, the `Authorization` header is created by encoding the username and password in Base64 format using the `btoa()` function. The resulting string is then prefixed with the word "Basic" to indicate that basic authentication is being used.

The API will then validate the credentials and return the requested data in JSON format, which is then logged to the console. Note that this example is using a GET request, but the same concept can be applied to other HTTP methods such as POST, PUT, or DELETE.

## Deep Dive:

Basic authentication has been around since the early days of the World Wide Web, and has since been replaced by more secure methods such as OAuth. However, it is still widely used due to its simplicity and ease of implementation.

Alternative methods of securing web applications and APIs include token-based authentication and certificate-based authentication. These methods provide a higher level of security and allow for more granular access control.

When implementing basic authentication in Javascript, it is important to note that the `fetch()` function is not supported by all browsers. In these cases, you can use third-party libraries or the traditional `XMLHttpRequest` object to make the HTTP request.

## See Also:

- [MDN web docs: Basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [The evolution of web authentication methods](https://www.lucidchart.com/techblog/2018/07/31/the-evolution-of-web-authentication-methods/)
- [Securing APIs with OAuth](https://www.oauth.com/)