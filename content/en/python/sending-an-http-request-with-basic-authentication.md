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

Sending an HTTP request with basic authentication involves adding an 'Authorization' header to the request. Programmers do this to access web services that require user validation, making sure only authorized personnel gets in.

## How to:

In Python, you can use the `requests` library to send an HTTP request with basic authentication. Here's how:

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://api.github.com/user', auth=HTTPBasicAuth('username', 'password'))
print(response.json())
```
The output will be your user data in a JSON format if the username and password are correct. In case of incorrect credentials, you'll get the relevant error message.

## Deep Dive

HTTP Basic Authentication has been there since the dawn of the world wide web. Back then, its simplicity and effective method of user validation made it widely adopted.   
Alternative methods include Digest Access Authentication, which is a tad more secure, and Bearer Token method, widely used in OAuth 2.0.
Basic Auth smoothly transforms into these alternatives when you need to up the authentication game in your app. 

While sending a basic auth request in Python, your username and password are base64 encoded. Note that this is not in any way encryption. The credentials can be easily decoded using the base64 encoding. That’s why it's mostly preferable to use an HTTPS connection to ensure your credentials don't get exposed.

## See Also:

To learn more, check out these articles:
- [HTTP Basic Auth](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Digest Access Authentication](https://en.wikipedia.org/wiki/Digest_access_authentication)
- [HTTP Authentication from Mozilla](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Python requests library](http://docs.python-requests.org/en/latest/) 

Just remember, with Basic Auth, always use HTTPS. Happy coding!