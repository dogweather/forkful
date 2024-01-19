---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a webpage means fetching its entire HTML content. This is useful when you want to process data from the page, automate actions on the site, or even cache a pristine copy.

## How to:
For TypeScript, we can use modules like `axios` (a HTTP client) and `cheerio` (a back-end jQuery version). Here's the how-to:

```TypeScript
import axios from "axios";
import * as cheerio from "cheerio";

const downloadWebPageContent = async (url: string) => {
    const { data: htmlContent } = await axios.get(url);
    return cheerio.load(htmlContent);
};

// Usage
(async () => {
    const $ = await downloadWebPageContent("https://example.com");
    console.log(`Title of the page: ${$("head > title").text()}`);
})();
```

Running this fetches the HTML from Example.com and logs its title.

## Deep Dive
Historically, downloading a webpage meant using low-level network libraries (like sockets) to manually send HTTP requests and parse responses. It's much easier today with HTTP clients like `axios`.

You may also see `node-fetch`, which mirrors the Fetch API in the browser but with additional Node.js functionality. Still, `axios` is often preferred due to more comprehensible error handling.

Remember - the method of download depends on how the webpage is rendered. While our example handles static pages, dynamic JS-rendered pages might require techniques like headless browsing (Puppeteer, anyone?).

## See Also
For a deeper understanding, check these:

- Axios (https://github.com/axios/axios)
- Cheerio (https://github.com/cheeriojs/cheerio)
- Fetch API (https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Puppeteer (https://github.com/puppeteer/puppeteer)