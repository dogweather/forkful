---
title:                "Sending an http request with basic authentication"
html_title:           "PHP recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

### What & Why?
Sending an HTTP request with basic authentication is a method used by programmers to access secure resources on a web server that require authentication. It involves sending a username and password with the request, allowing the server to verify the credentials and grant access to the requested resource.

### How to:
To send an HTTP request with basic authentication in PHP, you can use the `curl` function with the `CURLOPT_HTTPHEADER` option. The code below shows how to make a request to a secure resource with basic authentication:
```
<?php 
// Set your username and password
$username = "username";
$password = "password";

// Set the request URL
$url = "https://example.com/secure-resource";

// Initialize curl and set options
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPHEADER, array('Authorization: Basic '. base64_encode($username . ':' . $password)));

// Make the request
$response = curl_exec($ch);

// Check for errors
if(curl_errno($ch)){
  echo 'Error: ' . curl_error($ch);
}

// Close curl
curl_close($ch);

// Print the response
echo $response;
?>
```
The output of this code will be the response from the requested resource, assuming the credentials were correct.

### Deep Dive:
Sending an HTTP request with basic authentication has been a common practice since the early days of the internet, when security was not as advanced as it is today. It is a simple and widely supported way of authenticating users and granting access to secure resources.
Alternatives to basic authentication include using a more secure encryption method, such as OAuth, or implementing a custom authentication process. However, basic authentication is still widely used due to its simplicity and ease of implementation.

When implementing basic authentication, it is important to ensure that the username and password are sent over a secure connection, such as HTTPS, to prevent them from being intercepted by malicious users. It is also recommended to use strong, unguessable passwords to increase security.

### See Also:
- [PHP CURL documentation](https://www.php.net/manual/en/book.curl.php)
- [HTTP Authentication: Basic and Digest Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OAuth](https://oauth.net/)