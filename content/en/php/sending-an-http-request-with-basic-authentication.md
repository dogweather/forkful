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

## Why
Sending an HTTP request with basic authentication allows users to securely access restricted resources on a web server by sending login credentials along with the request. This is useful for applications that require user authentication.

## How To
To send an HTTP request with basic authentication using PHP, you will need to use the "curl" library and provide the necessary login credentials. Here's an example:

```PHP
// Specify the URL of the resource
$url = "https://example.com/secret-resource";

// Create a new cURL resource
$ch = curl_init($url);

// Set the option for basic authentication
curl_setopt($ch, CURLOPT_USERPWD, "username:password");

// Execute the request and obtain the response
$response = curl_exec($ch);

// Check for errors
if(curl_errno($ch)){
    echo 'Error: ' . curl_error($ch);
}

// Close cURL resource
curl_close($ch);

// Output the response
echo $response;
```

The "CURLOPT_USERPWD" option sets the username and password for basic authentication. These credentials will be sent along with the request to the specified URL. If the request is successful, the response will be stored in the "response" variable. You can then use this response in your application as needed.

## Deep Dive
When sending an HTTP request with basic authentication, the username and password are Base64 encoded and sent in clear text. This means that they can be easily intercepted and decoded by anyone with access to the network. This method should only be used for low-risk applications where security is not a major concern.

For higher security, it is recommended to use SSL/TLS encryption along with basic authentication. This will encrypt the credentials and prevent them from being intercepted. Additionally, it is important to use strong and unique passwords to ensure the security of your application.

## See Also 
- [PHP curl library documentation](https://www.php.net/manual/en/book.curl.php)
- [Sending HTTP requests in PHP](https://www.php.net/manual/en/function.curl-exec.php)
- [Understanding basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Secure password practices](https://www.techrepublic.com/blog/it-security/10-tips-for-creating-a-secure-password/)