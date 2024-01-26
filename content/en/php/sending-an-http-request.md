---
title:                "Sending an HTTP request"
date:                  2024-01-20T18:00:24.778839-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is the process by which a program asks for data from a server. Programmers do it to interact with web services, APIs, or to simply fetch web page content.

## How to:

PHP's got a neat way to handle HTTP requests with the `cURL` library. But the newer kid on the block is using `file_get_contents` for simpler GET requests, or the `stream_context_create` for POST requests. Here's a quick look at both.

### GET Request with file_get_contents():
```php
// The URL you're hitting up
$url = "http://example.com/api";

// Use file_get_contents to perform a GET request
$response = file_get_contents($url);

// Dump output to see what you got
var_dump($response);
```

### POST Request with stream_context_create():
```php
// The URL you're posting to
$url = "http://example.com/api";

// The data you're sending
$data = http_build_query([
    'foo' => 'bar',
    'baz' => 'qux',
]);

// Stream context options
$options = [
    'http' => [
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => $data,
    ],
];

// Create a stream context
$context  = stream_context_create($options);

// Perform the POST request and put the response in a variable
$result = file_get_contents($url, false, $context);

// See what you've received
var_dump($result);
```

## Deep Dive

Back in the day, `fsockopen()` was the go-to for PHP HTTP requests. It was clumsy, but it got the job done. Then along came `cURL`, still powerful and widely used, especially for complex operations. But sometimes, you don't need a chainsaw to cut a piece of string. That's where `file_get_contents()` and `stream_context_create()` shine. 

One key thing about `file_get_contents()` is it’s simplicity. Perfect for simple GET requests. But what if you need to POST data? Enter `stream_context_create()`. This little gem lets you fine-tune your HTTP requests with headers, methods, and more.

Under the hood, `file_get_contents()` and `stream_context_create()` use PHP’s stream wrappers. These replace the low-level socket operations handled by `fsockopen()`.

One drawback? Error handling can be trickier. If something goes south, these functions are less forgiving than `cURL`. If you need detailed response info or have to deal with complex HTTP tasks, consider sticking with `cURL`.

## See Also

- cURL's official PHP doc: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)
- PHP stream contexts: [https://www.php.net/manual/en/context.php](https://www.php.net/manual/en/context.php)
- HTTP context options: [https://www.php.net/manual/en/context.http.php](https://www.php.net/manual/en/context.http.php)
