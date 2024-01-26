---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:44:51.808510-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/downloading-a-web-page.md"
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
