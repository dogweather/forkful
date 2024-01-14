---
title:                "PHP recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests with basic authentication is a crucial aspect of web development as it allows secure communication between a client and server. Basic authentication is a simple yet effective method of verifying the identity of a user before granting access to a restricted page or API.

## How To

To send an HTTP request with basic authentication, we first need to set up a basic authentication string by combining the username and password with a colon (":") separator, and encoding it in Base64 format. This string will be used as the value for the "Authorization" header in the request.

```
<?php
// Set the credentials for basic authentication
$username = "john";
$password = "secretpassword";
$auth_string = base64_encode($username . ":" . $password);

// Set the URL of the endpoint to make the request to
$url = "https://example.com/api/v1/users";

// Set up the request headers with the authorization string
$headers = array(
    "Authorization: Basic " . $auth_string
);

// Make the request using cURL
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$output = curl_exec($ch);
curl_close($ch);

// Print the output of the request
echo $output;
?>
```

The above example uses cURL to make an HTTP GET request to an endpoint that requires basic authentication. We set the authentication string in the request headers and make the request to the specified URL. The output of the request will be stored in the `$output` variable and can be used as desired.

## Deep Dive

Although basic authentication is a simple and straightforward method of authenticating users, it is not considered the most secure option as the credentials are sent in plain text. This means that anyone with access to the request headers can decode the base64 string and retrieve the username and password.

To improve the security of basic authentication, it is recommended to always use HTTPS when making HTTP requests. This ensures that the credentials are encrypted during transmission, making it much harder for anyone to intercept and decode them.

Another way to enhance the security of basic authentication is to use a salt and hash function to encrypt the password before sending it in the header. This adds an extra layer of security and ensures that even if the base64 string is decoded, the actual password cannot be retrieved.

## See Also

- [cURL PHP documentation](https://www.php.net/manual/en/book.curl.php)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)