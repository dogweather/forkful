---
title:                "Parsing html"
html_title:           "PHP recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Parsing HTML is a key skill for any web developer. It allows you to extract specific data and elements from a HTML document, making it easier to manipulate and use in your code. Whether you are scraping data from a website or building a web crawler, learning how to parse HTML is essential.

## How To

To start parsing HTML in PHP, you will need to use a library called *PHP Simple HTML DOM Parser*. This library provides an easy to use interface for manipulating HTML elements.

First, import the library into your PHP file by requiring the `simple_html_dom.php` file:

```PHP
require 'simple_html_dom.php';
```

Next, we will need to create a new instance of the `simple_html_dom` class and load our HTML document:

```PHP
$html = new simple_html_dom();
$html->load_file('example.html');
```

Now we can use this instance to perform various operations on our HTML document. Let's say we want to extract all the links from our page and print them out:

```PHP
foreach ($html->find('a') as $link) {
    echo $link->href . PHP_EOL;
}
```

This will loop through all `<a>` tags in our HTML and print out their `href` attribute. The `PHP_EOL` constant adds a new line after each link, for clearer output.

To access specific elements, we can use CSS selectors with the `find()` method. For example, if we want to get the text inside a `<h1>` tag, we can use:

```PHP
$title = $html->find('h1', 0)->innertext;
echo $title;
```

This will print out the text inside the first `<h1>` tag in our HTML document.

## Deep Dive

PHP Simple HTML DOM Parser is based on the popular DOM (Document Object Model) library, which represents HTML documents as a tree structure. This makes it easy to navigate and manipulate elements by using its methods and properties.

Besides finding elements by their tag name, you can also use CSS selectors to target specific elements and attributes. For example, `#id` will select an element with a specific ID and `.class` will select elements with a specific class.

Additionally, the library provides methods for manipulating and modifying HTML elements. For example, you can use the `outertext` property to change the entire HTML of an element, or the `innertext` property to change only the text inside an element.

It's also worth noting that the library supports advanced features like advanced CSS selectors, traversing the DOM tree, and handling malformed HTML.

## See Also

- [PHP Simple HTML DOM Parser documentation](https://simplehtmldom.sourceforge.io/)
- [DOM library documentation](https://www.php.net/manual/en/book.dom.php)
- [Introduction to HTML DOM](https://www.w3schools.com/js/js_htmldom.asp)