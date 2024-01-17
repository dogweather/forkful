---
title:                "Sending an http request"
html_title:           "TypeScript recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for a frontend or backend application to communicate with a web server, usually over the internet. Programmers use HTTP requests to retrieve data from a server, submit form data, or make changes to data stored on the server.

## How to:

In TypeScript, we can use the `fetch()` function to send HTTP requests. It takes in a URL and returns a `Promise` that resolves with a `Response` object. Let's take a look at a simple example:

```TypeScript
fetch('https://example.com/users')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```

This code sends a GET request to the `users` endpoint on `example.com` and logs the response data to the console. We use the `then()` and `catch()` methods to handle the asynchronous response in a more readable way.

To make a POST request with data, we can pass in an options object as the second parameter:

```TypeScript
const user = { name: 'John', age: 25 };
const options = {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify(user)
};
fetch('https://example.com/users', options)
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log(error));
```

In this example, we set the request method to POST, specify the content type as JSON, and pass in the user object as the request body. The server can then read this data and process it accordingly.

## Deep Dive

HTTP (Hypertext Transfer Protocol) has been used for communication on the web since the early 1990s. It is a request-response protocol, which means that a client sends a request to a server, and the server responds with a message containing the requested data. This protocol is the foundation of the World Wide Web and enables us to access and share information over the internet.

There are also other ways to send data over the web, such as using WebSockets or WebRTC, but HTTP remains the most common and widely supported method.

In addition to using the `fetch()` function, developers can also choose to use third-party libraries like Axios or jQuery to make HTTP requests. These libraries often provide more advanced features, such as automatic parsing of response data or error handling.

## See Also

- [MDN Web Docs: HTTP Overview](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [Axios GitHub Repository](https://github.com/axios/axios)
- [jQuery AJAX Documentation](https://api.jquery.com/category/ajax/)