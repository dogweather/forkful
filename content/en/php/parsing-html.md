---
title:                "Parsing HTML"
date:                  2024-01-20T15:32:58.885783-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means to process and analyze the structure and content of HTML code. Programmers do it to scrape websites, manipulate or extract data, automate testing, or integrate web content into applications.

## How to:
PHP has a couple of built-in libraries to parse HTML, like DOMDocument. Here's a simple usage example:

```php
<?php
$htmlString = '<!DOCTYPE html><html><body><h1>Hello, World!</h1></body></html>';
$dom = new DOMDocument();
@$dom->loadHTML($htmlString); // '@' suppresses warnings caused by invalid HTML structures
$h1Tags = $dom->getElementsByTagName('h1');

foreach ($h1Tags as $tag) {
    echo $tag->nodeValue; // Output: Hello, World!
}
?>
```

This scripts outputs: `Hello, World!`

## Deep Dive
Back in the early web days, we grabbed HTML with regex and ad-hoc solutions, but it was messy. Enter `DOMDocument` and `SimpleXMLElement`, with proper HTML and XML parsing since PHP 5. They let you navigate and manipulate HTML as a tree structure.

Nowadays, while `DOMDocument` is your go-to for handling internal parsing, alternatives like `SimpleHTMLDom` and `phpQuery` provide additional syntactic sugar and can be friendlier for those coming from a JavaScript/jQuery background.

Internally, `DOMDocument` converts HTML to a DOM tree, making it easy to access specific elements, change attributes, and even modify the document on-the-fly. One cool thing about `DOMDocument` is its tolerance for poor HTML, cleaning it up and letting you work with real-world web pages that aren't always perfectly formatted.

## See Also
- [DOMDocument on PHP.net](https://www.php.net/manual/en/class.domdocument.php)
- [SimpleXML for handling basic XML tasks](https://www.php.net/manual/en/book.simplexml.php)
- [simplehtmldom SourceForge Project](https://sourceforge.net/projects/simplehtmldom/)
- [phpQuery GitHub repository](https://github.com/punkave/phpQuery)