---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a crucial aspect of web development and automation. It allows you to interact with web servers and retrieve data from APIs, making it an essential skill for any developer or systems administrator.

## How To

To send an HTTP request in Bash, you can use the `curl` command. Here's an example of a basic GET request:

```Bash
curl https://example.com
```

This will retrieve the homepage of `example.com` and print the HTML response to your terminal. You can also specify the HTTP method, headers, and data in your request. For example:

```Bash
curl -X POST -H "Content-Type: application/json" -d '{"username": "john", "password": "abc123"}' https://example.com/login
```

In this case, we're making a POST request with a JSON body to a login endpoint. The response will contain the data we receive after logging in.

You can also use variables and loops to simplify sending multiple HTTP requests. For instance, if you have a list of URLs stored in a file called `urls.txt`, you can iterate through them and make a GET request to each one using the following script:

```Bash
#!/bin/bash
while read -r url; do
  curl $url
done < urls.txt
```

This will save you time and effort instead of manually typing the `curl` command for each URL.

## Deep Dive

Behind the scenes, the `curl` command utilizes the HTTP protocol to communicate with a server. It forms a request with a request line, headers, and a body (if applicable), and then receives a response with a status line, headers, and a body.

To further customize your HTTP request, you can specify options such as the user-agent, cookies, authentication methods, and even proxy settings. You can find a full list of options and their usage in the `curl` manual page.

In addition to `curl`, you can also use other tools like `wget` or `httpie` to send HTTP requests in Bash. Each tool may have its own set of features and options, so it's worth exploring and comparing them to find the one that best suits your needs.

## See Also

- `curl` manual page: https://curl.se/docs/manpage.html
- `wget` manual page: https://www.gnu.org/software/wget/manual/wget.html
- `httpie` documentation: https://httpie.io/docs