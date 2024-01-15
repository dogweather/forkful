---
title:                "基本認証を使用したhttpリクエストの送信"
html_title:           "Python: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

HTTP requests with basic authentication allow for secure communication between a client (such as a web browser) and a server. This is especially important when sensitive information, such as login credentials, needs to be transmitted.

## How To

To send an HTTP request with basic authentication in Python, we first need to import the `requests` library. Then, we can use the `requests.get` method with the `auth` parameter set to a tuple containing the username and password.

```Python
import requests

url = "https://www.example.com"
username = "user123"
password = "secret"

response = requests.get(url, auth=(username, password))
print(response)
```

This will return a `Response` object, which contains information about the request and response, including the status code and any headers.

## Deep Dive

When using basic authentication, the username and password are encoded in the request header. This encoding is called "Basic authentication" and is not considered secure, as the credentials can be easily decoded. Therefore, it is recommended to use more advanced authentication methods, such as OAuth, for sensitive information.

## See Also
- [Requests library documentation](https://requests.readthedocs.io/en/master/)
- [HTTP Basic authentication explanation](https://www.httpwatch.com/httpgallery/authentication/#basicauth)
- [OAuth authentication in Python](https://requests-oauthlib.readthedocs.io/en/latest/oauth1_workflow.html)