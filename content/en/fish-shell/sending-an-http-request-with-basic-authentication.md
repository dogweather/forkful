---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending HTTP requests with basic authentication is a common practice in modern web development. It allows for secure communication between a client and server through the use of a username and password.

## How To
To send an HTTP request with basic authentication in Fish Shell, we first need to install the "httpie" command line tool. This can be done using the following command:

```Fish Shell
brew install httpie
```

Once installed, we can use the "http" command to send a request with basic authentication. Here's an example using the "get" method to request information from a server:

```Fish Shell
http --auth username:password GET https://example.com
```

The "--auth" flag is used to specify the credentials for basic authentication in the format of "username:password". The above command will return the response from the server in JSON format.

## Deep Dive
While the above example is a simple way to send an HTTP request with basic authentication, it's important to note some additional details. The username and password provided in the credentials can be encoded using base64 before being transmitted, which adds an extra layer of security. Also, keep in mind that basic authentication is not the most secure method of authentication and should be used with caution.

## See Also
- [Official Fish Shell Documentation for HTTPie](https://fishshell.com/docs/current/cmds/httpie.html)
- [HTTPie GitHub Repository](https://github.com/jakubroztocil/httpie)
- [Understanding Basic and Digest Authentication](https://www.httpwatch.com/httpgallery/authentication/)