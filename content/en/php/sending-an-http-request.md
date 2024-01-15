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

## Why
When building a website or web application, PHP developers often need to interact with external APIs or retrieve data from other web servers. Sending an HTTP request is an essential skill for achieving this, as it allows communication between a client and a server.

## How To
Sending an HTTP request in PHP is a straightforward process that involves building a request, sending it to a server, and receiving a response. Let's take a look at a simple example:

```
<?php

// Create a new HTTP request
$request = curl_init('https://example.com/api/users/1');

// Set request options
curl_setopt($request, CURLOPT_RETURNTRANSFER, true);

// Send the request and store the response in a variable
$response = curl_exec($request);

// Close the request
curl_close($request);

// Output the response
echo $response;
```

In this example, we use the `curl` library to build an HTTP request to retrieve data from an API. First, we use the `curl_init()` function to create a new request and pass in the URL of the endpoint we want to access. Then, we set any necessary options for the request using `curl_setopt()`. In this case, we use `CURLOPT_RETURNTRANSFER` to make sure the response is returned as a string rather than being output directly to the browser.

Next, we use `curl_exec()` to send the request to the server and store the response in a variable. Finally, we close the request using `curl_close()` and output the response to the browser.

The above example is a basic demonstration of sending an HTTP request, but there are many more options and configurations that can be used. It's essential to read the documentation and understand the different options available to customize your request based on your specific needs.

## Deep Dive
Behind the scenes, sending an HTTP request involves using a request method (such as GET, POST, PUT, or DELETE) and sending any necessary data (parameters, headers, etc.) along with the request. The server then processes the request and sends back a response, typically containing a status code and any relevant data.

In our previous example, we used the `curl` library, but PHP also has built-in functions for sending HTTP requests, such as `file_get_contents()` and `fopen()`. These functions also allow you to specify additional options and configurations for your requests.

It's important to note that sending an HTTP request can also involve authentication, which is necessary for accessing certain APIs or sensitive data. This is typically done by including an authorization header in the request that contains a token or credentials.

See Also
- [PHP cURL Documentation](https://www.php.net/manual/en/book.curl.php)
- [PHP: Hypertext Preprocessor](https://www.php.net/)
- [HTTP Requests in PHP: A Beginner’s Guide](https://www.pluralsight.com/guides/php-http-requests-beginners-guide)