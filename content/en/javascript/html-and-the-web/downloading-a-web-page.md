---
title:                "Downloading a web page"
aliases:
- /en/javascript/downloading-a-web-page.md
date:                  2024-01-20T17:44:18.382025-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means grabbing the HTML, CSS, JavaScript, and any other data that makes up the page from the server. Programmers do it to parse content, automate interactions, or archive web pages.

## How to:

Here's a quick way to download a page using Node.js with `node-fetch`:

```Javascript
const fetch = require('node-fetch'); // you might need to install this first!

async function downloadPage(url) {
    try {
        const response = await fetch(url);
        const body = await response.text();
        console.log(body); // Outputs the HTML source of the page
    } catch (error) {
        console.error(error);
    }
}

downloadPage('https://example.com');
```

Sample output:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive

Historically, downloading a web page was done with XMLHTTPRequest in the browser or `http` module in Node.js. However, post-ES6, `fetch` API became the modern standard due to its easier syntax and promise-based nature.

Alternatives include `axios`, a popular npm package, which handles requests with a bit more functionality than native fetch. For complex use cases, you might use `puppeteer` to actually render the page in a headless browser, useful for dealing with JavaScript-rendered content.

When implementing page downloads, pay attention to aspects like respecting `robots.txt`, handling `User-Agent` to avoid getting blocked, and managing asynchronous handling carefully to dodge potential pitfalls with server overload or race conditions.

## See Also

- MDN Web Docs on the `fetch` API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch
- Axios GitHub page: https://github.com/axios/axios
- Puppeteer GitHub page: https://github.com/puppeteer/puppeteer
- An article on web scraping best practices: https://www.scrapingbee.com/blog/web-scraping-best-practices/
