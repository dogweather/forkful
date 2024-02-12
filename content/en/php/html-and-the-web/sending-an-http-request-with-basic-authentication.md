---
title:                "Sending an HTTP request with basic authentication"
aliases:
- en/php/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:04.901464-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves adding a username and password to access a resource on a server. Programmers use it because some APIs and web services require authentication to ensure only authorized users access their data.

## How to:

Here's the simple way to send an HTTP request with basic authentication using cURL in PHP:

```PHP
<?php
$url = 'https://api.example.com/data';
$username = 'your_username';
$password = 'your_password';

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

Sample output:

``` 
{
  "authenticated": true,
  "data": "Some secure data"
}
```

## Deep Dive

HTTP Basic Authentication has been in use since the early days of the web. It's not the most secure option around (as credentials are sent in base64 encoding, which is easily decoded), but it's straightforward to implement for quick-and-dirty access control.

Suppose security is a concern (and it should be), you’d turn to more robust methods like OAuth, JWT or API keys. Yet, basic auth persists partly due to legacy systems and partly for internal systems where you control access tightly.

In PHP, cURL is widely used for making HTTP requests, but alternatives like `file_get_contents` or Guzzle (a PHP HTTP client) exist. When using `file_get_contents`, a context with the appropriate header must be created:

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$response = file_get_contents($url, false, $context);

echo $response;
?>
```

Picking the right tool comes down to your project’s needs and the level of control and functionality you desire.

## See Also

To dive deeper and expand your knowledge, check these out:

- [cURL documentation](https://www.php.net/manual/en/book.curl.php)
- [Guzzle documentation](http://docs.guzzlephp.org/en/stable/)
- [PHP `file_get_contents` function](https://www.php.net/manual/en/function.file-get-contents.php)
- [HTTP authentication with PHP](https://www.php.net/manual/en/features.http-auth.php)
