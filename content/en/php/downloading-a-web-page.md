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

## Why

Ever wanted to save a webpage for offline viewing or reference? You can easily do so by downloading the webpage using PHP. This can come in handy when you have limited internet access or need to refer to a webpage frequently without having to load it every time.

## How To

To download a webpage using PHP, you need to follow these steps:

1. First, we need to initialize a new cURL session using the `curl_init()` function. This allows us to send and receive HTTP requests.

```PHP
$curl = curl_init();
```

2. Next, we set the URL of the webpage we want to download using the `curl_setopt()` function.

```PHP
curl_setopt($curl, CURLOPT_URL, 'https://www.example.com');
```

3. We also need to set the `CURLOPT_RETURNTRANSFER` option to true, which will return the response data instead of printing it.

```PHP
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
```

4. Now, we can execute the cURL request and save the response data into a variable using the `curl_exec()` function.

```PHP
$response = curl_exec($curl);
```

5. Finally, we can close the cURL session using the `curl_close()` function.

```PHP
curl_close($curl);
```

The `response` variable now contains the webpage's HTML code, which can be saved into a file for offline viewing.

## Deep Dive

cURL (Client URL Library) is a popular library for performing URL-related operations like downloading webpages. It supports various protocols like HTTP, HTTPS, FTP, and more. You can also set various options, headers, and authentication methods using cURL. For advanced usage, you can refer to the official cURL documentation.

There are also other PHP libraries and packages available, like Guzzle and file_get_contents, that can be used to download webpages. However, cURL is the recommended option for its flexibility and widespread usage.

## See Also

- [cURL documentation](https://www.php.net/manual/en/book.curl.php)
- [Guzzle library](https://docs.guzzlephp.org/en/stable/)
- [file_get_contents function](https://www.php.net/manual/en/function.file-get-contents.php)