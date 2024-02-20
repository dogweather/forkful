---
date: 2024-01-20 17:59:48.745761-07:00
description: "Sending an HTTP request is how your JavaScript code talks to a server.\
  \ It's done to exchange data, fetch resources, or send data to the server for\u2026"
lastmod: 2024-02-19 22:05:18.892263
model: gpt-4-1106-preview
summary: "Sending an HTTP request is how your JavaScript code talks to a server. It's\
  \ done to exchange data, fetch resources, or send data to the server for\u2026"
title: Sending an HTTP request
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is how your JavaScript code talks to a server. It's done to exchange data, fetch resources, or send data to the server for processing. 

## How to:

JavaScript uses the `fetch` API for sending requests. Here’s how to do a simple GET request:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Error:', err));
```

The output will be JSON data from the URL. Easy, right?

And for a POST request:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  body: JSON.stringify({
    title: 'foo',
    body: 'bar',
    userId: 1,
  }),
  headers: {
    'Content-type': 'application/json; charset=UTF-8',
  },
})
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Error:', err));
```

This sends new data and outputs the server's response.

## Deep Dive

HTTP requests have been around since the beginning of the web—think HTML forms. XMLHttpRequest (XHR) was once the go-to method for sending requests in JavaScript, but it's clunky. 

Enter `fetch`, a modern approach that's promise-based, making it cleaner and more robust. Unlike XHR, `fetch` handles both requests and responses in a single, unified API and is built into the language, no libraries required.

Alternatives? Sure. Libraries like Axios or jQuery's Ajax are still in use. They offer some syntactic sugar and workarounds for specific quirks, but `fetch` is native and generally the way forward.

Implementation details? Remember to handle errors, work with different response types, and be aware of cross-origin resource sharing (CORS) rules. 
    
## See Also

- MDN `fetch` API Documentation: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Using promises in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
- Learn about CORS: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
