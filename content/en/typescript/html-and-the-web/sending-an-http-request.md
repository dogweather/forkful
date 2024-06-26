---
date: 2024-01-20 18:00:32.545074-07:00
description: 'How to: In TypeScript, you''d typically use the Fetch API to send HTTP
  requests. Here''s a quick example, using `async/await` for simplicity.'
lastmod: '2024-03-13T22:44:59.856012-06:00'
model: gpt-4-1106-preview
summary: In TypeScript, you'd typically use the Fetch API to send HTTP requests.
title: Sending an HTTP request
weight: 44
---

## How to:
In TypeScript, you'd typically use the Fetch API to send HTTP requests. Here's a quick example, using `async/await` for simplicity:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetch error:', error);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

Sample output for a successful request:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive
HTTP requests have been paramount since the dawn of the web; they're how browsers and servers chat. Before `fetch` came XMLHttpRequest (XHR), which did the job but felt like paperwork. `fetch`, a modern alternative, is promise-based, cleaner, and part of the window object in most modern browsers.

Alternatives to `fetch` in TypeScript include libraries like Axios, which provide more features and are sometimes easier to handle. Axios automatically transforms JSON data, handles request cancellation, and offers better error handling.

Behind the scenes, TypeScript compiles down to JavaScript. When you send an HTTP request using `fetch`, you're essentially using the browser's native Fetch API. TypeScript's type-checking augments your code stability by catching type errors at compile-time.

## See Also
- MDN Web Docs on Fetch: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Axios GitHub Repo: https://github.com/axios/axios
- A comparison of HTTP request libraries: https://www.npmtrends.com/axios-vs-fetch
