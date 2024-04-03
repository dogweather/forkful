---
date: 2024-01-20 18:00:20.079942-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA:) Here\u2019\
  s a quick code snippet using `requests` in Python."
lastmod: '2024-03-13T22:44:38.630335-06:00'
model: gpt-4-1106-preview
summary: "Here\u2019s a quick code snippet using `requests` in Python."
title: "\u05E9\u05DC\u05D9\u05D7\u05EA \u05D1\u05E7\u05E9\u05EA HTTP"
weight: 44
---

## How to: (איך לעשות:)
Here’s a quick code snippet using `requests` in Python:

```python
import requests

# Get request
response = requests.get('https://api.github.com')
print(response.status_code)
print(response.json())

# Post request
payload = {'key1': 'value1', 'key2': 'value2'}
post_response = requests.post('https://httpbin.org/post', data=payload)
print(post_response.text)
```
Sample output:

```
200
{'current_user_url': 'https://api.github.com/user', ... etc ... }
{
  "args": {}, 
  "data": "", 
  "files": {}, 
  "form": {
    "key1": "value1",
    "key2": "value2"
  },
  ... etc ...
}
```

## Deep Dive (עומק השקעה)
HTTP has been around since 1991 - Tim Berners-Lee's invention. Today, besides `requests`, you might see `http.client` or third-party packages like `httpx`. Each has its purpose. `requests` is friendly and high-level; `http.client` offers lower-level control. When sending HTTP requests, consider what data you send. With `POST`, data is in the body; `GET` goes in the URL.

## See Also (ראה גם)
Learn more from these:

- Requests documentation: [http://docs.python-requests.org](http://docs.python-requests.org)
- HTTPX, an async alternative: [https://www.python-httpx.org](https://www.python-httpx.org)
- Python’s own HTTP library: [https://docs.python.org/3/library/http.client.html](https://docs.python.org/3/library/http.client.html)
