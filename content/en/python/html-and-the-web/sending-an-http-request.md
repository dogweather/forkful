---
date: 2024-01-20 18:00:10.605176-07:00
description: 'How to: Python''s third-party `requests` library makes HTTP calls a
  breeze. Below is how to send a simple GET request.'
lastmod: '2024-03-13T22:44:59.705350-06:00'
model: gpt-4-1106-preview
summary: Python's third-party `requests` library makes HTTP calls a breeze.
title: Sending an HTTP request
weight: 44
---

## How to:
Python's third-party `requests` library makes HTTP calls a breeze. Below is how to send a simple GET request:

```python
import requests

response = requests.get('https://api.example.com/data')
print(response.status_code)  # Outputs the status code of the response
print(response.json())      # If response carries JSON, prints it as a Python dict
```

More detailed POST request with JSON payload and custom headers:

```python
import requests
import json

url = "https://api.example.com/submit"
data = {'key': 'value'}
headers = {'Content-Type': 'application/json'}

response = requests.post(url, data=json.dumps(data), headers=headers)

print(response.status_code)
print(response.json())
```

## Deep Dive
HTTP requests are how the web works — they've been around since the early 90s. Alternatives to Python's `requests` include the standard library's `urllib`, but it's a bit more cumbersome.

Understanding how to send HTTP requests involves knowing methods (GET, POST, PUT, DELETE, etc.), status codes (e.g., 200 OK, 404 Not Found), headers, and body data. 

For streaming or asynchronous requests, you might explore `requests`' async counterpart or the `aiohttp` package. Underneath, these libraries use Python's `socket` for raw network communication.

Historically, `requests` is considered a go-to due to its simplicity and power, but `httpx`, a newer async-compatible library, is gaining traction.

## See Also
- The `requests` library documentation: https://requests.readthedocs.io
- HTTP status codes explained: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- Python's `urllib` documentation: https://docs.python.org/3/library/urllib.html
- `httpx` library for async HTTP requests: https://www.python-httpx.org
