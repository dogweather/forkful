---
date: 2024-01-20 17:44:51.808510-07:00
description: "Downloading a web page means fetching its contents over the Internet.\
  \ Programmers do this to interact with web content, automate tasks, or extract\u2026"
lastmod: '2024-03-11T00:14:22.702902-06:00'
model: gpt-4-1106-preview
summary: "Downloading a web page means fetching its contents over the Internet. Programmers\
  \ do this to interact with web content, automate tasks, or extract\u2026"
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Downloading a web page means fetching its contents over the Internet. Programmers do this to interact with web content, automate tasks, or extract information.

## How to (Як це зробити):

To download a web page in TypeScript, you'll need `node-fetch`. Install it with `npm install node-fetch`. Here's how you use it:

```typescript
import fetch from 'node-fetch';

async function downloadPage(url: string): Promise<string> {
    try {
        const response = await fetch(url);
        return response.ok ? await response.text() : 'Failed to load page';
    } catch (error) {
        return 'Error: ' + error;
    }
}

downloadPage('https://www.example.com').then(console.log);
```

Sample output for a successful page load might be the HTML contents, and for an error, 'Error: TypeError: Failed to fetch'.

## Deep Dive (Поглиблене Вивчення):

Historically, downloading a web page was often done with XMLHttpRequest in the browser, or with modules like `http` or `https` in Node.js. `fetch` API provides a cleaner, promise-based way to make web requests, and `node-fetch` is essentially node's implementation of what `fetch` does in the browser.

Alternatives to `node-fetch` include `axios` or libraries like `got`. Each has its pros and cons in terms of features, syntax, and performance.

Understanding HTTP status codes and headers is crucial. They tell you if your request was successful (`200` series), redirected (`300` series), had client errors (`400` series), or server errors (`500` series).

## See Also (Див. також):

- MDN Web Docs on Fetch API: [https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Comparison of HTTP request libraries: [https://www.npmjs.com/package/node-fetch#fetch-vs-axios-vs-other-http-libraries](https://www.npmjs.com/package/node-fetch#fetch-vs-axios-vs-other-http-libraries)
- HTTP Status Codes: [https://httpstatuses.com/](https://httpstatuses.com/)
