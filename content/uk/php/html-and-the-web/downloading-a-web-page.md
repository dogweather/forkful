---
title:                "Завантаження веб-сторінки"
aliases:
- uk/php/downloading-a-web-page.md
date:                  2024-01-20T17:44:25.533331-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Downloading a web page means grabbing the HTML content of a page via HTTP. Programmers do it to interact with web data, scrape content, or automate tests.

## How to: (Як це зробити:)
```PHP
<?php
$url = "http://example.com";
$ch = curl_init($url);

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$pageContent = curl_exec($ch);

if ($pageContent === false) {
    echo "Curl error: " . curl_error($ch);
} else {
    echo $pageContent;
}

curl_close($ch);
?>
```
**Sample output:** 
The actual HTML content of http://example.com.

## Deep Dive (Поглиблений Розбір)
Back in the day, `file_get_contents()` was popular for simple requests. Now, cURL (Client URL Library) is the go-to tool. It handles complex HTTP requests, supports various protocols, and allows for fine-grained control over headers, cookies, and more. Its versatility trumps older methods. When using cURL, remember to set the appropriate options, like `CURLOPT_RETURNTRANSFER` to ensure the result is returned, not printed. Handle errors gracefully by checking if `$pageContent` is false, then use `curl_error()` for diagnostic info.

Alternatives to consider, like PHP's `file_get_contents()` for simple GET requests, or the Guzzle library for a more robust HTTP client, if dealing with a complex project where you need advanced features like persistent connections, asynchronous requests, or handling streams.

## See Also (Дивіться Також)
- PHP cURL Manual: https://www.php.net/manual/en/book.curl.php
- Guzzle Documentation: http://docs.guzzlephp.org/en/stable/
- PHP.net Stream Context options: https://www.php.net/manual/en/context.php

(Keep exploring and happy coding!) - (Досліджуйте далі і кодуйте з задоволенням!)
