---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage is grabbing a copy of its HTML, CSS, and other data. Programmers do this for various reasons, like creating offline versions of sites, scraping the data for analysis, or automated testing of web functionalities.

## How to:

In JavaScript, you can use several methods to download a webpage. One common library for this is `Axios`, due to its promise-based, browser-compatible feature set. Here's an example:

```Javascript
const axios = require('axios');
const fs = require('fs');

axios.get('http://example.com')
  .then(function (response) {
    fs.writeFileSync('example.html', response.data);
  })
  .catch(function (error) {
    console.log(error);
  });
```
Running this script will create a copy of the `example.com` home page as `example.html` in your script’s directory, or print an error to the console.

## Deep Dive:

Historically, fetching web content in JavaScript was more complex. You'd have to deal with the low-level `XMLHttpRequest` API and callback functions. However, higher-level libraries like `axios`, `node-fetch` or `request` have abstracted most of the complexities, allowing for simplified, promise-based syntax.

There are also different strategies for downloading web pages, like streaming the data for large pages or concurrent downloading for several pages at once. It's important to choose the right toolset and strategy depending on the use case.

Keep in mind, downloading a webpage simply fetches its static content. For dynamic pages with a lot of JavaScript or SPA's (single page applications) like those built with React or Vue, you might need more complex tools like headless browsers.

## See Also:

- [Axios documentation](https://axios-http.com/docs/intro)
- [Node.js File System documentation](https://nodejs.org/api/fs.html)
- [MDN Web Docs guide to using Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [Puppeteer, a Node.js library for headless Chrome browser automation](https://pptr.dev)