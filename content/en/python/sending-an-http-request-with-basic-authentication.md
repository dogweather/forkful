---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Python and HTTP Requests: A Crash Course
## What & Why?
At its core, sending an HTTP request with basic authentication is a way to interact with web services that require username/password credentials. We essentially pack up say "Hello, I'm 'username' and my password is 'password'", right in the HTTP request. Programmers do this to pull, push, or manipulate data from these web services.

## How to:
Python's requests library makes HTTP requests a breeze. Here’s a basic implementation:

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://httpbin.org/basic-auth/user/passwd', auth=HTTPBasicAuth('user', 'passwd'))

print(response.status_code)
print(response.json())
```

Running this code, you should receive:

```
200
{'authenticated': True, 'user': 'user'}
```

This assumes the webservice at `https://httpbin.org/basic-auth/user/passwd` requires basic auth using 'user' as username and 'passwd' as password.


## Deep Dive
The use of HTTP basic authentication goes way back to the early days of the web. Back when the internet was a new frontier, basic auth provided an easy method of securing a web page or web service.

As for alternatives, more secure methods of authentication have been developed over the years including Digest, token-based, and OAuth. Basic auth has its limitations (credentials sent in plain text), but continues to be used due to its simplicity.

On the implementation details, the `requests.get()` function sends a GET request to the URL defined. The optional `auth` parameter is used to handle HTTP basic authentication. 

## See Also
For further information, check out these links:

- [Python requests library documentation](https://docs.python.org/3/library/http.client.html)
- [W3C docs on HTTP Basic Auth](http://www.w3.org/Protocols/HTTP/1.0/spec.html#BasicAA)
- [HTTPbin for testing your requests](https://httpbin.org)