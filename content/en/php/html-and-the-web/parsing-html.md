---
title:                "Parsing HTML"
aliases: - /en/php/parsing-html.md
date:                  2024-02-03T19:02:45.807993-07:00
model:                 gpt-4-0125-preview
simple_title:         "Parsing HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML in PHP involves extracting specific information from HTML documents. Programmers perform this task to automate data extraction, web scraping, or to integrate content from various web pages within their applications, enhancing functionality without manual intervention.

## How to:
For parsing HTML, PHP programmers can utilize built-in functions or lean on robust libraries like Simple HTML DOM Parser. Here, we'll explore examples using both PHP's `DOMDocument` and the Simple HTML DOM Parser.

### Using `DOMDocument`:
PHP's `DOMDocument` class is a part of its DOM extension, allowing for parsing and manipulating HTML and XML documents. Here's a quick example on how to use `DOMDocument` to find all the images in an HTML document:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>Sample Page</title>
</head>
<body>
    <img src="image1.jpg" alt="Image 1">
    <img src="image2.jpg" alt="Image 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

Sample output:
```
image1.jpg
image2.jpg
```

### Using Simple HTML DOM Parser:
For more complex tasks or easier syntax, you might prefer using a third-party library. Simple HTML DOM Parser is a popular choice, providing a jQuery-like interface for navigating and manipulating HTML structures. Here's how to use it:

First, install the library using Composer:
```
composer require simple-html-dom/simple-html-dom
```

Then, manipulate HTML to, for example, find all links:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

This code snippet will fetch the HTML content of 'http://www.example.com', parse it, and print out all the hyperlinks. Remember to replace `'http://www.example.com'` with the actual URL you wish to parse.

Utilizing these methods, PHP developers can effectively parse HTML content, tailor data extraction to their needs, or seamlessly integrate external web content into their projects.
