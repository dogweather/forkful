---
title:                "Sending an http request"
html_title:           "PHP recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request means sending a message to a web server in order to request information or resources. Programmers do this in order to retrieve data from a web server, such as a website or an API.

## How to:

To send an HTTP request in PHP, you can use the `file_get_contents()` or `curl()` functions. These functions accept a URL as the first parameter and return the response from the server. For example:

```PHP
$response = file_get_contents('http://www.example.com');
echo $response; // outputs the HTML of the website
```

If you need to add specific headers or set options for your request, you can use the `stream_context_create()` function with the `file_get_contents()` function. This allows for more customization of your request. For example:

```PHP
$context = stream_context_create([
	'http' => [
		'header' => "Accept: application/json\r\n",
		'method' => 'GET'
	]
]);

$response = file_get_contents('http://www.example.com/api', false, $context);
echo $response; // outputs the JSON response from the API
```

## Deep Dive:

HTTP requests have been around since the early days of the internet and are a core functionality for web development. PHP offers several options for sending HTTP requests, including the `file_get_contents()` and `curl()` functions mentioned above.

Other programming languages may have their own methods for sending HTTP requests, but PHP's built-in functions provide a simple and straightforward way to make requests. Additionally, there are many useful libraries and packages available for more advanced HTTP requests in PHP, such as Guzzle and Symfony HTTP Client.

Some things to keep in mind when sending HTTP requests are error handling and security. It's important to properly handle any potential errors that may occur during the request and to secure your requests by using HTTPS when sending sensitive data.

## See Also:

- PHP `file_get_contents()` documentation: https://www.php.net/manual/en/function.file-get-contents.php
- PHP `curl()` documentation: https://www.php.net/manual/en/book.curl.php
- Guzzle HTTP Client: https://docs.guzzlephp.org/en/stable/
- Symfony HTTP Client: https://symfony.com/doc/current/http_client.html