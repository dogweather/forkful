---
date: 2024-01-20 18:00:13.880328-07:00
description: Sending an HTTP request means asking a server for data or sending data
  to it. Programmers do it to interact with web services, fetch content, or push data.
lastmod: '2024-03-13T22:44:48.578271-06:00'
model: gpt-4-1106-preview
summary: Sending an HTTP request means asking a server for data or sending data to
  it. Programmers do it to interact with web services, fetch content, or push data.
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## What & Why? (Що і Чому?)
Sending an HTTP request means asking a server for data or sending data to it. Programmers do it to interact with web services, fetch content, or push data.

## How to: (Як це зробити:)
```Python
import requests

# Get request
response = requests.get('https://api.github.com')
print(response.status_code)
print(response.json())

# Post request
data = {'key': 'value'}
response = requests.post('https://httpbin.org/post', json=data)
print(response.status_code)
print(response.json())
```
Output:
```
200
{'current_user_url': 'https://api.github.com/user', ...}

200
{
  "args": {}, 
  "data": "{\"key\": \"value\"}", 
  "files": {}, 
  "form": {}, 
  "headers": {
    ...
  }, 
  ...
}
```

## Deep Dive (Поглиблений Аналіз)
HTTP requests have been around since the early '90s, an essential part of web browsing. Python's `requests` library makes requests easy: you use `get` to fetch data and `post` to send it. There are alternatives, like `http.client` for lower-level operations and the `urllib` library, but they are more verbose and complex. `requests` handles many implementation details like encoding parameters and managing responses.

## See Also (Дивіться також)
- Official `requests` library documentation: https://requests.readthedocs.io
- HTTP protocol overview on MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP
- Python `http.client` docs: https://docs.python.org/3/library/http.client.html
- Python `urllib` tutorial: https://docs.python.org/3/howto/urllib2.html
