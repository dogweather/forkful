---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
HTTP requests are the cornerstone of data exchange on the web, enabling your PHP script to communicate with a server. You send HTTP requests to post data to a server, fetch data for your app, or update existing data.

## How to:
To send HTTP requests in PHP, we can use either cURL or file_get_contents. 

Below is an example using cURL:
```PHP
$url = "http://example.com";
$ch = curl_init(); 
curl_setopt($ch, CURLOPT_URL, $url); 
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$result = curl_exec($ch); 
curl_close($ch);
echo $result;
```
The output will be the contents of the http://example.com page.

Here's an example using file_get_contents:
```PHP
$url = "http://example.com";
$result = file_get_contents($url);
echo $result;
```
Just like the cURL example, the output is the contents of http://example.com.

## Deep Dive
Historically, PHP developers used cURL to send HTTP requests. But with the introduction of file_get_contents in PHP 4.3.0, we got a more straightforward way to get the file contents, including HTTP data. 

If your request is simple, use file_get_contents. For advanced options like specifying headers for the request or sending POST data, cURL is the way to go. 

Implementation-wise, the cURL method brings overhead, initializing and configuring a cURL session with curl_init() and curl_setopt(). On the other hand, file_get_contents is a simple function call.
 
There's also a fresh player in town: GuzzleHttp, a PHP HTTP client that makes HTTP requests much easier. Its best parts? Error handling and the ability to send asynchronous requests.

## See Also
*PHP.net cURL*: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)

*PHP.net file_get_contents*: [https://www.php.net/manual/en/function.file-get-contents.php](https://www.php.net/manual/en/function.file-get-contents.php)

*Guzzle Documentation*: [http://docs.guzzlephp.org/en/stable/](http://docs.guzzlephp.org/en/stable/)

Short but juicy, that's it for HTTP requests in PHP. Happy coding!