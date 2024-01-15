---
title:                "Parsing html"
html_title:           "TypeScript recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Why

If you're a frontend developer, you've probably encountered the need to parse HTML at some point. Whether it's extracting data from a web page or manipulating the DOM, HTML parsing is an essential skill to have. With TypeScript, you can easily and safely parse HTML using its built-in features.

## How To

To parse HTML in TypeScript, you can make use of the `DOMParser` class. This allows us to create an HTML document from a string of HTML code. Let's take a look at an example:

```TypeScript
const htmlString = '<h1>Hello, world!</h1>';
const parser = new DOMParser();
const htmlDoc = parser.parseFromString(htmlString, 'text/html');
console.log(htmlDoc.body.innerHTML);
```
Output:

```html
<h1>Hello, world!</h1>
```

In the code above, we first create a string containing our HTML code. Then, we instantiate a `DOMParser` and use the `parseFromString()` method to convert the string into an HTML document. We specify the `text/html` parameter to indicate that we're parsing HTML. Finally, we can access the HTML document's body and log the inner HTML to the console.

Note that the `DOMParser` class is available in most modern browsers, so there's no need for any external libraries or dependencies.

## Deep Dive

Besides extracting data and manipulating the DOM, HTML parsing can also be useful for sanitizing user input. By parsing the HTML and removing any malicious tags or attributes, we can prevent cross-site scripting (XSS) attacks.

The `DOMParser` class also allows for more advanced parsing options, such as parsing a specific section of an HTML document or parsing XML documents. Check out the documentation for more information on these options.

It's worth mentioning that there are also other libraries and tools available for parsing HTML in TypeScript. For example, `htmlparser2` and `cheerio` are popular choices for server-side HTML parsing.

## See Also

- [DOMParser - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [htmlparser2 - npm](https://www.npmjs.com/package/htmlparser2)
- [cheerio - npm](https://www.npmjs.com/package/cheerio)