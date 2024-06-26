---
date: 2024-01-20 17:44:25.533331-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) **Sample output:** The actual HTML content of http://example.com."
lastmod: '2024-04-05T21:53:49.594405-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ **Sample output:** The actual HTML content of http://example.com."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

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
