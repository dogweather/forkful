---
title:                "Sending an http request with basic authentication"
html_title:           "Python recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

When sending an HTTP request with basic authentication, programmers are including a set of credentials in the request header in order to access protected resources. This method of authentication is commonly used for simple applications that do not require high levels of security. 

## How to:

```Python
import requests

url = "https://www.example.com/api/protected_resource"
username = "username"
password = "password"

r = requests.get(url, auth=(username, password))

print(r.status_code)
```
Output: 200

## Deep Dive:

### Historical Context:

HTTP basic authentication has been around since the early days of the internet, and was originally designed to be a simple and easy way to authenticate users for web applications. 

### Alternatives:

While basic authentication is still commonly used, it is not considered the most secure option. Alternatives such as OAuth and token-based authentication have been developed to provide more secure methods of authentication.

### Implementation Details:

When sending an HTTP request with basic authentication, the username and password must be encoded in a specific format and added to the request header. The server then checks these credentials against the ones stored on its end to allow access to the resource. 

## See Also:

- [HTTP Authentication: Basic and Digest Access Authentication | Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OAuth Official Website](https://oauth.net/)
- [Token-Based Authentication for WebAPIs: ASP.NET Cors + Custom Delegating Handler](https://docs.microsoft.com/en-us/archive/blogs/azureossds/token-based-authentication-for-webapis-asp-net-cors-custom-delegating-handler)