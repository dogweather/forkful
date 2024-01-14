---
title:                "Bash recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Have you ever wondered how websites and applications securely handle user credentials? One common way is through basic authentication, which involves sending an HTTP request with a username and password. In this blog post, we'll explore how to do this using Bash programming.

## How To
To send an HTTP request with basic authentication using Bash, we can use the `curl` command. Here's an example of how to do this:

```Bash
curl -u username:password https://example.com
```

The `-u` flag allows us to specify a username and password in the format `username:password`. In this example, we're sending a GET request to `https://example.com` with the provided credentials.

We can also use variables to store the username and password, making our code more dynamic. Here's an example of how to do this:

```Bash
# Store username and password in variables
username="John"
password="SecretPassword"

# Send HTTP request with basic authentication using variables
curl -u "$username:$password" https://example.com
```

This way, we can easily change the credentials without having to modify the command itself.

We can also include additional options in our `curl` command, such as specifying the HTTP method or adding headers to the request. Here's an example of how to do this:

```Bash
# Specify the HTTP method and add a custom header
curl -u username:password -X POST -H "Content-Type: application/json" https://example.com/api
```

This will send a POST request with the specified headers to the API endpoint.

## Deep Dive
So how does sending an HTTP request with basic authentication actually work? When we specify the username and password in the `curl` command, the credentials are encoded in Base64 and added to the request header as `Authorization: Basic <encoded_credentials>`. This allows the server to identify and verify the user's identity.

It's worth noting that basic authentication is not the most secure method for handling user credentials, as the credentials are sent in plain text and can potentially be intercepted. As a result, it's important to use HTTPS when implementing basic authentication to ensure that the credentials are encrypted during transmission.

## See Also
- [Using cURL to test RESTful APIs](https://blog.restcase.com/how-to-use-curl-to-test-rest-api/)
- [Introduction to Basic Authentication](https://www.loginradius.com/blog/async/basic-authentication/)
- [Security Considerations for Basic Authentication](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html#basic-authentication)

By now, you should have a better understanding of how to send an HTTP request with basic authentication using Bash. Remember to always use HTTPS when implementing this method for added security. Happy coding!