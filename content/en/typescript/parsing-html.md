---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML in TypeScript: An Informal Guide 

Let's navigate the journey of parsing HTML using TypeScript together in this quick, easy tutorial.

## What & Why?

Parsing HTML refers to converting it from a string into a structured format that we can interact with programmatically. We do this to enable information extraction, manipulation, and the embedding of scripts, especially in web scraping and web automation.

## How to:

Here's a simple way to parse HTML with the help of jsdom library (version 16.6.0 as of writing). Let's get started:

```TypeScript
import { JSDOM } from 'jsdom';

function parseHTML(htmlString: string): void {
  const dom = new JSDOM(htmlString);
  console.log(dom.window.document.querySelector("h1").textContent);
}
```
To use this, simply input an HTML string and run the function. It will output the content of any `h1` tags it finds.

Example:

```typescript
parseHTML("<h1>Hello, world!</h1>")
//Outputs: Hello, world!
```

## Deep Dive

Now, let's delve into the depths.

jsdom emerged in the late 2000s as an effort to provide a pure-JavaScript implementation of the DOM that can be used outside of a browser. It has since continued to adapt to changes and advancements in the web platform.

While jsdom is widely used due to its completeness and ease of use, other libraries such as "cheerio" are often desirable due to their simplicity and speed. Cheerio is a lean implementation of core jQuery; it's faster than jsdom, but builder for HTML manipulation not full-scale web-page emulation.

In jsdom's implementation, the parser uses the htmlparser2 library under the hood, which converts the HTML string into a tree-like structure that mirrors the HTML's tag structure. Meanwhile, cheerio uses parse5 which provides nearly exactly HTML5 spec-comparable parse and serialize capabilities.

## See Also

For more details, you can dig further using some of these links:

- TypeScript official Handbook [here](https://www.typescriptlang.org/docs/handbook/intro.html)
- jsdom on GitHub [here](https://github.com/jsdom/jsdom)
- cheerio on GitHub [here](https://github.com/cheeriojs/cheerio)
- parse5 on GitHub [here](https://github.com/inikulin/parse5)
- htmlparser2 on GitHub [here](https://github.com/fb55/htmlparser2)

With that, you are now geared up to start parsing HTML using TypeScript. Happy coding!