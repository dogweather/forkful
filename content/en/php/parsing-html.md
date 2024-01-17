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

## What & Why?

Parsing HTML is the process of extracting data from HTML code and converting it into a structured format that can be easily manipulated and used by a computer. Programmers do this to automate tasks and data extraction, making their work more efficient and accurate.

## How to:

To parse HTML using PHP, we can use the built-in functions ```file_get_contents()``` and ```DOMDocument```. 

First, we use ```file_get_contents()``` to retrieve the HTML code from a URL or a local file. Then, we use ```DOMDocument``` to parse the HTML and extract the desired data.

``` php
// Retrieve HTML code
$html = file_get_contents('http://www.example.com');

// Create a new DOMDocument object
$dom = new DOMDocument();

// Load the HTML code into the DOMDocument object
$dom->loadHTML($html);

// Get all <a> tags from the HTML code
$links = $dom->getElementsByTagName('a');

// Loop through each <a> tag and print the href attribute value
foreach ($links as $link) {
    echo $link->getAttribute('href');
}
```

Sample output:
``` 
http://www.example.com
http://www.google.com
http://www.facebook.com
```

## Deep Dive:

Parsing HTML has been a common practice among programmers since the early days of the internet. In the past, this was done using regular expressions, which can be error-prone and difficult to maintain. With the introduction of the DOMDocument class in PHP 5, parsing HTML has become much easier and more reliable.

An alternative to using ```file_get_contents()``` and ```DOMDocument``` is to use a third-party library such as Simple HTML DOM or phpQuery. These libraries offer more advanced features and are often used for web scraping or building web crawlers.

It's also important to note that HTML is a flexible and dynamic language, making parsing it a challenging task. Different HTML structures and attributes can make it difficult to write a one-size-fits-all solution. Therefore, it's important to carefully test and adjust the parsing code for different scenarios.

## See Also:

- [PHP official documentation for file_get_contents() function](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP official documentation for DOMDocument class](https://www.php.net/manual/en/class.domdocument.php)
- [Simple HTML DOM](https://simplehtmldom.sourceforge.io/)
- [phpQuery](https://code.google.com/archive/p/phpquery/)