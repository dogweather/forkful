---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Basic authentication in HTTP is a method used to provide username and password while making a request to a server. Programmers employ this when they want to communicate securely with APIs or other services that demand user credentials.

## How to:

Here's how to send an HTTP request with basic authentication using PHP. We'll use `curl` because it's powerful and bundled with PHP.

```PHP
<?php
$url = 'https://your-api-example.com';
$userName = 'SomeUserName';
$password = 'SomePassWord';

// Initialize a cURL session
$curl = curl_init();

// Set the cURL options
curl_setopt($curl, CURLOPT_URL, $url);
curl_setopt($curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($curl, CURLOPT_USERPWD, "$userName:$password");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);

// Make the request and save response to $response
$response = curl_exec($curl);

// Close request to clear up some resources
curl_close($curl);

echo $response;
?>
``` 
Assuming that the specifics (URL, username, password) are valid, you'll see the response from your API request on your screen.

## Deep Dive

Basic HTTP Authentication is a vintage-nifty protocol from the early days of the web. It's been around since 1996 as a part of the HTTP/1.0 spec. However, it's far from obsolete — in fact, it's used oftentimes, especially with RESTful APIs.

Yes, alternatives do exist. OAuth is one popular choice, providing token-based authentication instead of sending the username and password each time. Another more lightweight option is token-based authentication, which could be based on JSON Web Tokens (JWT).

Implementation-wise, keep in mind that basic authentication transmits credentials in base64 encoding, which is not a secure form of encryption. Therefore, it's absolutely crucial to use this method over HTTPS, which provides the necessary end-to-end encryption. Without HTTPS, you risk exposing your sensitive login credentials.

## See Also

For further reading and various approaches to this topic, visit:

- PHP Manual’s guide on cURL: https://www.php.net/manual/en/book.curl.php
- HTTP Authentication methods: https://www.iana.org/assignments/http-authschemes/http-authschemes.xhtml
- JSON Web Tokens: https://jwt.io/introduction/
- The OAuth 2.0 Authorization Framework: https://tools.ietf.org/html/rfc6749.

Remember, PHP is a powerful language with highly flexible and dynamic features. Picking up an additional method to work with HTTP requests broadens your horizons and gives you one more valuable tool in your coding toolkit.