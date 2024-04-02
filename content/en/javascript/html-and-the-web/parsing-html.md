---
changelog:
- 2024-01-28, dogweather, reviewed
date: 2024-01-20 15:32:10.298214-07:00
description: "Parsing HTML means extracting data from HTML documents. Programmers\
  \ do it to interact with or manipulate web content, automate data extraction, or\
  \ for web\u2026"
lastmod: '2024-03-13T22:45:00.431554-06:00'
model: unknown
summary: "Parsing HTML means extracting data from HTML documents. Programmers do it\
  \ to interact with or manipulate web content, automate data extraction, or for web\u2026"
title: Parsing HTML
weight: 43
---

## What & Why?
Parsing HTML means extracting data from HTML documents. Programmers do it to interact with or manipulate web content, automate data extraction, or for web scraping purposes.

## How to:
Let's parse HTML using the `DOMParser` API in JavaScript. 

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Output: Hello, world!
```

Now, let’s grab something more specific, like an element with a class:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Output: Hello, again!
```

## Deep Dive
Parsing HTML is old as the web. Initially, it was a browser thing—browsers parsed HTML to display web pages. Over time, programmers wanted to tap into this process, leading to APIs like `DOMParser`.

Alternatives? Sure. We've got libraries like `jQuery` and tools like `BeautifulSoup` for Python. But JavaScript’s native `DOMParser` is fast and built-in, no need for extra libraries.

Implementation-wise, when you parse HTML with `DOMParser`, it creates a `Document` object. Think of it as a hierarchical model of your HTML. Once you have it, you can navigate and manipulate it just like you would with a normal web page’s DOM.

Here's the thing—parsing can trip on malformed HTML. Browsers are forgiving, but `DOMParser` might not be. Hence, for complex tasks or messy HTML, third-party libraries might do a better cleanup job.

## See Also
- MDN Web Docs on the `DOMParser` API: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery’s parsing capabilities: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, a fast, flexible & lean implementation of core jQuery for the server: [Cheerio.js](https://cheerio.js.org/)
- For non-JS parsing: Python’s BeautifulSoup library: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
