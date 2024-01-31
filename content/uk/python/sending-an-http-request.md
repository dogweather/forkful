---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T18:00:13.880328-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

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
