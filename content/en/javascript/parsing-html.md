---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML with Javascript

## What & Why?

Parsing HTML is the act of breaking down an HTML document into its constituent elements for further processing or analysis. Programmers do this to interact with, understand, or manipulate the structure and content of web pages.

## How to:

JavaScript provides built-in functions to parse HTML.

```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString('<p><em>Hello</em>, web world!</p>', 'text/html');
console.log(doc.body.textContent); // "Hello, web world!"
```
The output of the above program will be:

```Hello, web world!```

This will help you parse an HTML string and output plain text.

## Deep Dive

HTML parsing isn't new— browsers have done this since the dawn of the web. Initially, JavaScript didn't offer any built-in HTML parsing capabilities, but the DOMParser API was eventually introduced to fill this gap.

There are other methods for parsing HTML in JavaScript, like the 'createContextualFragment'. It's a method that can load HTML into the range object, which then can be appended to the document.

One critical aspect of parsing HTML is handling malformed HTML. The DOMParser API handles this gracefully, making the code error-resistant.

Implementation-wise, HTML parsing involves tokenization (breaking down the HTML into tokens, aka the constituent parts), then constructing a document model with these tokens. This process is quite complex but is abstracted away in built-in JavaScript methods.

## See Also

_MDN documentation_ - [DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)

_Javascript.info tutorial_ - [Modifying document](https://javascript.info/modifying-document)

_MDN documentation_ - [Document.createRange](https://developer.mozilla.org/en-US/docs/Web/API/Document/createRange) and then [Range.createContextualFragment](https://developer.mozilla.org/en-US/docs/Web/API/Range/createContextualFragment)

_HTML parsing specification_ - [HTML Living Standard](https://html.spec.whatwg.org/multipage/parsing.html#parsing)

Remember, parsing HTML in JavaScript is a powerful tool in your programming arsenal, but with great power comes great responsibility! Always use these methods responsibly and ethically. Happy coding!