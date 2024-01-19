---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-html.md"
---

{{< edit_this_page >}}

# Parsing HTML in PHP: A Simple How-to Guide

## What & Why?

Parsing HTML is the process of converting HTML data into a structured format that's easier to interact with. It's critical for developers as it allows extracting, manipulating, and utilizing data from HTML documents.

## How to:

Let's breeze through a simplified snippet of code using PHP's built-in HTML parser, the DOMDocument class. 

```PHP
<?php   
$dom = new DOMDocument; 
libxml_use_internal_errors(true);
$dom->loadHTML('<p>Hello, World!</p>'); 
$paragraphs = $dom->getElementsByTagName('p');
foreach($paragraphs as $para) {
    echo $para->nodeValue, PHP_EOL; 
}  
?>
```

This code will result in the output:

```
Hello, World!
```

Pretty neat, right? This PHP script creates a new instance of the DOMDocument class, loads the HTML into it, and then iterates over each paragraph (element with 'p' tag), echoing its content.

## Deep Dive 

PHP has in-built functions for HTML parsing since its early days, reflecting how integral HTML document manipulations are. However, parsing can also be done using other techniques, like regular expressions or third-party libraries such as SimpleHTML or phpQuery. Balance is key, though; opt for in-built functions when they suffice since they are typically faster, consume less memory, and are more efficient at handling edge cases.

## See Also

- More on DOMDocument: https://www.php.net/manual/en/class.domdocument.php
- Learn about SimpleHTML: https://simplehtmldom.sourceforge.io/
- Dive into phpQuery: https://code.google.com/archive/p/phpquery/

This guide just scratched the surface of HTML parsing. Feel free to check out these resources to keep exploring!