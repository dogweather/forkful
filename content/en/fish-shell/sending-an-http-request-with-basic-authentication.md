---
title:                "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
HTTP requests are a crucial part of modern web development. Whether you're building a website or creating an API, being able to send HTTP requests is an essential skill to have. One common type of HTTP request is a request with basic authentication. It involves providing a username and password along with the request, allowing the server to verify the identity of the user. In this blog post, we will explore how to send an HTTP request with basic authentication using Fish Shell.

## How To
Sending an HTTP request with basic authentication is a simple process in Fish Shell. First, we need to install the `curl` package, which allows us to make HTTP requests from the command line. We can do this using the built-in package manager in Fish Shell:

```Fish Shell
$ fisher install curl
```

Next, we can use the `curl` command to send our request. We need to specify the URL we want to send the request to, along with the `-u` flag followed by the username and password separated by a colon. For example:

```Fish Shell
$ curl -u username:password https://example.com
```

This will send a GET request to the specified URL with the provided credentials. We can also specify the type of request using the `-X` flag, and include any necessary data using the `-d` flag. For example, if we wanted to send a POST request with data, we could use the following command:

```Fish Shell
$ curl -u username:password -X POST -d '{"name": "John", "age": 25}' https://example.com
```

## Deep Dive
Underneath the surface, sending an HTTP request with basic authentication involves encoding the username and password in the request header using the Base64 algorithm. This ensures that the credentials are not sent in plain text, improving security. Additionally, many APIs require basic authentication as a means of verifying the identity of the user before allowing access to certain resources.

## See Also
- Official Fish Shell documentation on sending HTTP requests with basic authentication: https://fishshell.com/docs/current/cmds/curl.html
- Tutorial on using cURL for making HTTP requests: https://www.digitalocean.com/community/tutorials/how-to-use-curl-to-make-http-requests
- Guide on HTTP authentication types: https://www.httpwatch.com/authentication/types/