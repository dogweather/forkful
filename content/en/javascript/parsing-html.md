---
title:                "Javascript recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Why

As a web developer, you might come across the need to extract specific information from HTML pages. This is where HTML parsing comes in handy. By parsing HTML, you can extract data and manipulate it in your JavaScript code to create dynamic and interactive web applications.

## How To

To start parsing HTML in JavaScript, you will first need to import a module called "node-html-parser". This will allow us to access the HTML parsing functionality.

```Javascript
// Import the module
const parse = require('node-html-parser').parse;
// Sample HTML code
const html = '<div class="container"><h1>Hello, world!</h1><p>This is a paragraph.</p></div>';
// Parse the HTML
const parsedHTML = parse(html);
// Get the text inside the <h1> tag
const title = parsedHTML.querySelector('h1').text;
// Get the text inside the <p> tag
const paragraph = parsedHTML.querySelector('p').text;
// Print the results
console.log(title); // Output: Hello, world!
console.log(paragraph); // Output: This is a paragraph.
```

In this example, we have imported the "node-html-parser" module and used its `parse` function to parse the HTML code. Then, we used `querySelector` to select specific elements from the parsed HTML and extract their text content.

## Deep Dive

HTML parsing involves breaking down an HTML document into smaller parts, such as elements, attributes, and text content. This allows us to access and manipulate specific parts of the HTML as needed.

The "node-html-parser" module uses a technique called DOM manipulation, which stands for Document Object Model. This means that the HTML document is represented as an object with various properties and methods, making it easier to access and manipulate.

To learn more about HTML parsing and DOM manipulation, you can refer to the official documentation for "node-html-parser" and the DOM APIs for JavaScript.

## See Also

- [node-html-parser documentation](https://www.npmjs.com/package/node-html-parser)
- [DOM APIs for JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)