---
title:                "Downloading a web page"
html_title:           "PHP recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

### What & Why?

Downloading a web page in PHP simply means retrieving the content of a web page from the internet using a programming language called PHP. Programmers do this because they often need to access and manipulate data from external websites in their applications.

### How to:

To download a web page in PHP, we can use the ```file_get_contents()``` function. This function takes in a URL as its parameter and returns the content of that URL as a string. Here's an example code:

```PHP
<?php
// Assign the URL of the web page to a variable
$url = 'https://www.example.com';

// Use the file_get_contents() function to retrieve the content
$content = file_get_contents($url);

// Output the content
echo $content;
?>
```

The output of this code will be the HTML of the web page, which we can then use in our application.

We can also use the ```curl``` library to download a web page in PHP. This library provides additional functionalities such as data transfer options and error handling. Here's an example code:

```PHP
<?php
// Initialize a new curl session
$curl = curl_init();

// Set the URL of the web page we want to download
curl_setopt($curl, CURLOPT_URL, 'https://www.example.com');

// Set return transfer option to true
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);

// Execute the request and store the response in a variable
$response = curl_exec($curl);

// Close the curl session
curl_close($curl);

// Output the response
echo $response;
?>
```

### Deep Dive:

PHP has built-in functions like ```file_get_contents()``` and libraries like ```curl``` that make downloading web pages easy. These functions and libraries use the HTTP protocol to send requests to web servers and retrieve the response.

In the past, developers used the ```fopen()``` function to download web pages, but this method has become obsolete due to security concerns. Another alternative is using the ```stream_context_create()``` function, which allows us to set additional parameters such as headers and authentication.

When downloading web pages, it's important to consider load time and server resources. If we need to frequently access the same web page, we can save the content locally to avoid excessive requests to the web server.

### See Also:

- PHP documentation for ```file_get_contents()```: https://www.php.net/manual/en/function.file-get-contents.php
- PHP documentation for ```curl```: https://www.php.net/manual/en/book.curl.php
- Alternatives to ```file_get_contents()```: https://stackoverflow.com/questions/7546017/php-fopen-vs-file-get-contents