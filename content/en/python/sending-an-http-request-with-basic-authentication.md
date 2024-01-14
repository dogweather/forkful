---
title:                "Python recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

In today's interconnected world, web applications have become a crucial part of our daily lives. These applications are built using the HTTP protocol for communication. In certain cases, these applications require users to provide authentication credentials before accessing certain resources. This is where sending HTTP requests with basic authentication becomes necessary.

## How To

Sending an HTTP request with basic authentication can be achieved using the `requests` library in Python. Let's take a look at a simple code snippet:

```Python
import requests

# Specify the URL of the endpoint that requires authentication
url = "https://example.com/login"

# Create a dictionary to store the username and password
auth = {'username': 'john', 'password': 'secret123'}

# Send a GET request with basic authentication
response = requests.get(url, auth=auth)

# Display the response code
print(response.status_code)
```

In the above code, we first import the `requests` library and specify the URL of the web application that requires basic authentication. Next, we create a dictionary to store the username and password of the user. Finally, we send a GET request to the specified URL with the authentication details included in the `auth` parameter. The response code is then printed, which will be `200` if the authentication is successful.

Here is a sample output of the code snippet:

```Python
200
```

## Deep Dive

When sending an HTTP request with basic authentication, the `requests` library automatically handles the process of encoding the username and password in Base64 and adding them to the request header. This ensures secure transmission of the credentials over the internet.

It's worth noting that basic authentication is not the most secure method of authentication, as the username and password are sent in plain text. It's recommended to use HTTPs to encrypt the communication and add an extra layer of security.

## See Also

- [Requests library documentation](https://docs.python-requests.org/en/master/)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [HTTP Protocol](https://developer.mozilla.org/en-US/docs/Web/HTTP)

Sending an HTTP request with basic authentication is a simple yet powerful way to access resources from a web application. It's important to consider the security implications and use appropriate measures to protect sensitive information. With the help of the `requests` library, handling basic authentication in Python is seamless and efficient.