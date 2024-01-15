---
title:                "פירוק html"
html_title:           "TypeScript: פירוק html"
simple_title:         "פירוק html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Why (למה)

Parsing HTML is an essential task for web developers who want to access, manipulate, or extract specific data from a webpage. It allows for more dynamic and interactive websites, enabling the creation of powerful web applications.

## How To (כיצד לעשות זאת)

Parsing HTML in TypeScript is made easy with the use of the popular library, Cheerio. First, install Cheerio using npm:

```TypeScript
npm install cheerio
```

Next, import Cheerio in your TypeScript file:

```TypeScript
import * as cheerio from 'cheerio';
```

Then, use the `load()` method to load your HTML string and access the DOM elements using CSS selectors:

```TypeScript
const $ = cheerio.load('<h1>Hello World</h1>');
const headline = $('h1').text();
```

The `headline` variable will now contain the text "Hello World". You can also use Cheerio to manipulate the HTML and extract specific data from multiple elements, making it a powerful tool for web scraping.

## Deep Dive (שקיפון לעומק)

Under the hood, Cheerio uses the popular jQuery library and provides similar syntax and functions. This makes it easy for developers familiar with jQuery to use Cheerio for HTML parsing in TypeScript.

Cheerio also has an extensive documentation with various utility methods for handling HTML, such as `html()`, `attr()`, and `find()`. It also supports different output formats, including XML and JSON.

Another advantage of using Cheerio is its speed and lightweight nature, making it a popular choice for projects that require efficient HTML parsing.

## See Also (ראה גם)

- Cheerio documentation: https://cheerio.js.org/
- TypeScript documentation: https://www.typescriptlang.org/
- jQuery documentation: https://api.jquery.com/