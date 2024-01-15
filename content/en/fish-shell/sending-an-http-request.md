---
title:                "Sending an http request"
html_title:           "Fish Shell recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request is a common task in today's world of web development. Whether you are building a web application, working with API's, or simply testing a website's functionality, knowing how to send HTTP requests using Fish Shell can greatly enhance your workflow.

## How To

To make an HTTP request using Fish Shell, you will need to use the `curl` command. This command allows you to specify the HTTP method, URL, and any additional parameters for your request.

```
Fish Shell

curl -X GET "https://www.example.com" 

```

In the example above, we are sending a GET request to the URL specified. This is the most common type of request, used for retrieving data from a server. However, there are many other HTTP methods that you can use such as POST, PUT, PATCH, and DELETE.

To include additional parameters in your request, you can use the `-d` flag and specify the data in the form of key-value pairs. For example:

```
Fish Shell

curl -X POST -d "username=john&password=12345" "https://www.example.com/login" 
```

This will send a POST request to the specified URL with the data of a username and password. The server can then use this data for authentication or other purposes.

You can also specify headers in your HTTP request using the `-H` flag. This is useful for providing necessary information to the server, such as the type of data you are sending. For example:

```
Fish Shell

curl -X POST -H "Content-Type: application/json" -d '{"name": "John", "age": 25}' "https://www.example.com/user"
```

In this example, we are sending a POST request with a JSON body to create a new user on the server.

## Deep Dive

There are many other options and flags that you can use with the `curl` command to customize your HTTP request. For example, you can include a user agent using the `-A` flag or set a timeout using the `-m` flag.

You can also use the `-i` flag to include the response headers in the output, or the `-o` flag to save the response to a file.

For more information on all the options and flags available for `curl`, you can refer to the official documentation.

## See Also

- Official `curl` documentation: https://curl.se/docs/manpage.html
- Fish Shell documentation: https://fishshell.com/docs/current/index.html