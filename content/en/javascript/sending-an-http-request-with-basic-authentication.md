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

## Why

If you are a web developer, you may have come across the need to send HTTP requests with basic authentication. This type of authentication is commonly used for secure communication between a client and server, and it ensures that only authorized users can access sensitive data.

## How To

Sending an HTTP request with basic authentication is fairly simple in Javascript. First, we need to create a new `XMLHttpRequest` object, which allows us to send and receive data from a server. We can do this using the `XMLHttpRequest()` constructor.

Next, we need to set the request method to `GET` or `POST` and specify the URL we want to request. We also need to set the `withCredentials` property to `true` to indicate that we want to use basic authentication.

Once that is done, we can set the `Authorization` header with the username and password separated by a colon, and then encode it using `btoa()`.

Here's an example of sending a GET request to a server with basic authentication:

```Javascript
let xhr = new XMLHttpRequest();
xhr.open('GET', 'https://example.com/api/users', true);
xhr.withCredentials = true;
xhr.setRequestHeader('Authorization', 'Basic ' + btoa('username:password'));
xhr.send();
```

Keep in mind that the server must support basic authentication for this to work.

## Deep Dive

Under the hood, basic authentication works by adding an `Authorization` header to the HTTP request. This header contains the word "Basic" followed by a base64-encoded string that represents the username and password. Here's an example of the header's value:

```
Basic dXNlcm5hbWU6cGFzc3dvcmQ=
```

In this example, the username is "username" and the password is "password". The `btoa()` function is used to encode this string in base64. On the server side, the username and password can be decoded using a base64 decoder and verified against a user database.

It's important to note that basic authentication is not the most secure method of authentication, as the username and password are transmitted in plain text. It's recommended to use a more secure method, such as OAuth, when dealing with sensitive data.

## See Also

- [XMLHttpRequest Documentation](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Using Fetch with Basic Authentication](https://developers.google.com/web/updates/2015/03/introduction-to-fetch)
- [Introduction to Basic Authentication](https://www.digitalocean.com/community/tutorials/understanding-basic-authentication-in-nginx-server-blocks)