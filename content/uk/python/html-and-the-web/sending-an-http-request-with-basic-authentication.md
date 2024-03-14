---
date: 2024-01-20 18:02:33.773894-07:00
description: "Sending an HTTP request with basic authentication means adding login\
  \ credentials to your web request for access. We do this to securely communicate\
  \ with\u2026"
lastmod: '2024-03-13T22:44:48.582581-06:00'
model: gpt-4-1106-preview
summary: "Sending an HTTP request with basic authentication means adding login credentials\
  \ to your web request for access. We do this to securely communicate with\u2026"
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Sending an HTTP request with basic authentication means adding login credentials to your web request for access. We do this to securely communicate with web services that require user verification.

## How to: (Як це зробити:)
```python
import requests
from requests.auth import HTTPBasicAuth

# Replace 'your_username' and 'your_password' with your actual credentials
username = 'your_username'
password = 'your_password'

# The URL you're sending the request to
url = 'https://api.example.com/data'

# Make the request with Basic Authentication
response = requests.get(url, auth=HTTPBasicAuth(username, password))

# Check the response
if response.ok:
    print('Success:', response.status_code)
    data = response.json()
    print(data)
else:
    print('Failed:', response.status_code)
```

Sample Output:
```
Success: 200
{'key': 'value', ...}
```

## Deep Dive (Поглиблений Аналіз):
Basic authentication isn't new; it's been around since the early days of the web. It's simple: encode your username and password in base64 and attach it to your request header. It’s not the safest, though, as credentials can be easily decoded if the connection isn't secure (use HTTPS!).

Alternatives like OAuth add more security but are also more complex. For APIs, tokens are often used instead of basic credentials.

In basic authentication, Python's `requests` library simplifies the process a lot. It handles the encoding and header setup. You don't touch the nitty-gritty – the library does it for you.

## See Also (Дивись також):
- [requests Documentation](https://requests.readthedocs.io/en/latest/)
- [The HTTP Authentication spec RFC 7617](https://tools.ietf.org/html/rfc7617)
- [Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
