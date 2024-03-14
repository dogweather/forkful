---
date: 2024-01-20 18:00:09.933363-07:00
description: "Sending an HTTP request is how your JavaScript code asks the internet\
  \ for data or sends data to a server. We do it to interact with web services, grab\u2026"
lastmod: '2024-03-13T22:44:49.987942-06:00'
model: gpt-4-1106-preview
summary: "Sending an HTTP request is how your JavaScript code asks the internet for\
  \ data or sends data to a server. We do it to interact with web services, grab\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Sending an HTTP request is how your JavaScript code asks the internet for data or sends data to a server. We do it to interact with web services, grab fresh content, submit forms, and basically breathe life into websites.

## How to: (Як зробити:)
Here's how to send a GET request using the `fetch` API to grab some JSON data and then post data using `fetch`.

```javascript
// GET request for remote image in node.js
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Error:', err));

// POST request using fetch()
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

## Deep Dive (Занурення у Деталі):
Long before `fetch`, we had `XMLHttpRequest`. It did the job but was clunky. `fetch` is the modern, promise-based evolution of web requests. Alternatives? Yes, many use `axios` for more features out of the box. Behind the scenes, `fetch` wraps around the low-level HTTP protocol, dealing with strings and streams neatly, so you don't have to.

Historically, sending requests from the browser was limited by the same-origin policy - a security measure. But now, with CORS (Cross-Origin Resource Sharing), we can request resources from different origins, given the server allows it.

## See Also (Дивіться Також):
- MDN Web Docs on `fetch()`: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- JSONPlaceholder for fake online REST: https://jsonplaceholder.typicode.com/
- `axios` GitHub repo: https://github.com/axios/axios
- CORS explanation by MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
