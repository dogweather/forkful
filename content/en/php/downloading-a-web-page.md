---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage is the process of obtaining data, specifically HTML, from a website's URL, storing it somewhere for later use. We, programmers, do it to access a website's data or to scrape desired content from the site.

## How to:

PHP provides several functions to download a webpage but the simplest one is perhaps `file_get_contents`.

```php
$url = 'https://example.com';
$page_contents = file_get_contents($url);
echo $page_contents;
```

When you run this, it will display the entire HTML content from `https://example.com`.

## Deep Dive

PHP wasn't originally designed do tasks like downloading web pages. It was a server-side scripting language for producing dynamic web pages. However, as the language evolved, it became useful for tasks like this.

There are other ways to download a webpage in PHP, such as using `cURL` library which provides more control, like setting headers, timeout, following redirect links.

```php
function get_web_page($url)
{
    $options = [
        CURLOPT_RETURNTRANSFER => true,  
        CURLOPT_HEADER         => false,   
        CURLOPT_FOLLOWLOCATION => true,   
        CURLOPT_MAXREDIRS      => 10,     
        CURLOPT_TIMEOUT        => 30,     
        CURLOPT_URL            => $url,    
    ];
    
    $ch = curl_init();
    curl_setopt_array($ch, $options);
    $content = curl_exec($ch);
    curl_close($ch);
    
    return $content;
}
```

This function will return the content of a given URL.

## See Also:

- Official PHP docs for `file_get_contents`: [here](https://www.php.net/manual/en/function.file-get-contents.php)
- `cURL` in the PHP docs: [here](https://www.php.net/manual/en/book.curl.php)