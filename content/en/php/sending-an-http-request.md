---
title:                "PHP recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a crucial aspect of web development. It allows us to communicate with external APIs and retrieve data, making our applications more dynamic and powerful. Without sending HTTP requests, our applications would be limited to just the data we have on our own servers.

## How To

To send an HTTP request in PHP, we can use the `file_get_contents()` function. This function takes in a URL as a parameter and returns the response as a string. Let’s take a look at an example:

```PHP
$response = file_get_contents('https://api.example.com/users');
echo $response;
```

In this code snippet, we are sending an HTTP GET request to the URL `https://api.example.com/users` and storing the response in a variable called `$response`. We then echo out the response, which can be in any format such as JSON, XML, or plain text.

We can also specify other parameters in the `file_get_contents()` function, such as the request method and headers. Let’s see an example of sending a POST request with a JSON body:

```PHP
$url = 'https://api.example.com/users';
$data = [
    'name' => 'John',
    'age' => 25
];

$options = [
    'http' => [
        'method' => 'POST',
        'header' => 'Content-Type: application/json',
        'content' => json_encode($data)
    ]
];

$context = stream_context_create($options);
$response = file_get_contents($url, false, $context);
echo $response;
```

In this example, we first define the URL we want to send the request to and the data we want to include in the request body. Then, we set the options for the request, including the request method (POST) and the content-type header. Finally, we create a stream context using the `stream_context_create()` function and pass in the options. We can then use this context as a parameter in the `file_get_contents()` function to send the POST request. We also specify `false` as the second parameter to prevent automatic redirects.

## Deep Dive

There are other methods and libraries available for sending HTTP requests in PHP, such as cURL and GuzzleHTTP. These offer more advanced functionality and options, such as handling authentication and setting custom headers.

It is also important to handle errors and exceptions when sending HTTP requests. We can use the `http_response_code()` function to check the status code of the response and handle any errors accordingly.

## See Also

- [PHP Documentation: file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP HTTP request libraries](https://www.php.net/manual/en/refs.webservice.php)