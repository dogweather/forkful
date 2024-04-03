---
date: 2024-01-20 17:59:48.745761-07:00
description: "How to: JavaScript uses the `fetch` API for sending requests. Here\u2019\
  s how to do a simple GET request."
lastmod: '2024-03-13T22:45:00.430643-06:00'
model: gpt-4-1106-preview
summary: JavaScript uses the `fetch` API for sending requests.
title: Sending an HTTP request
weight: 44
---

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
