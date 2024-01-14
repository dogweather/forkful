---
title:                "TypeScript recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to extract specific information from a website without manually sifting through its HTML code? Perhaps you're a web developer trying to scrape data for your project, or a curious internet user wanting to automate a tedious task. Whatever your reason may be, parsing HTML can save you time and effort. In this blog post, we'll discuss how you can use TypeScript to parse HTML and make your life easier.

## How To

To begin, let's first install the necessary dependencies. We'll be using the npm package "cheerio" for our HTML parsing needs. Open your command line and type in the following:

```TypeScript
npm install cheerio --save
```

Next, we'll create an HTML file named "index.html" with the following code:

```TypeScript
<html>
  <body>
    <h1>Welcome to my blog!</h1>
    <p>Here you'll find all kinds of interesting articles.</p>
  </body>
</html>
```

Now, in our TypeScript file, let's import cheerio and read our HTML file using the "fs" module:

```TypeScript
import * as cheerio from 'cheerio';
import * as fs from 'fs';

const html = fs.readFileSync('index.html', 'utf-8');
```

Next, we'll load our HTML code into cheerio and use its jQuery-like syntax to select specific elements:

```TypeScript
const $ = cheerio.load(html); //loads the HTML code

const title = $('h1').text(); //selects the text within the <h1> tag
const paragraph = $('p').text(); //selects the text within the <p> tag

console.log(`Title: ${title}`); //outputs "Welcome to my blog!"
console.log(`Paragraph: ${paragraph}`); //outputs "Here you'll find all kinds of interesting articles."
```

And just like that, we have successfully extracted specific information from our HTML code! Of course, this is a very simple example, but you can use the same approach for more complex HTML structures.

## Deep Dive

While cheerio makes parsing HTML a lot easier, it's important to understand how it actually works. Cheerio uses a lightweight, fast, and flexible HTML parser called "parse5" under the hood. Parse5 takes in HTML code and converts it into a tree-like structure. Cheerio then uses this structure to navigate and select specific elements.

One thing to keep in mind when using cheerio is that it doesn't execute JavaScript code. So if you need to extract data that is dynamically loaded through JavaScript, you'll need to use a headless browser like Puppeteer.

## See Also

- [Cheerio Documentation](https://cheerio.js.org/)
- [npm package "cheerio"](https://www.npmjs.com/package/cheerio)
- [Puppeteer Documentation](https://pptr.dev/)