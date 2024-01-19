---
title:                "Sending an http request"
html_title:           "Python recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is asking a server for some form of response. Coders do this to interact with web services, get data, or perform operations online.

## How to:

Python has its built-in `http` module, but using the `requests` module makes the task easier. Make sure you have it installed:

```Python
pip install requests
```

Then here's a basic GET request:

```Python
import requests

response = requests.get('https://www.example.com')

print(response.status_code)
print(response.text)
```
Output will display the status code (like '200' for success) and the webpage content.

Part of interacting with web services involves sending data in a POST request:
```Python
data = {'name':'John', 'job':'developer'}
response = requests.post('https://www.example.com', data=data)

print(response.status_code)
print(response.json())
```
This sends data to the server and also receives a response.

## Deep Dive

HTTP request methods have been around since the inception of web development. HTTP, or HyperText Transfer Protocol, is the set of rules guiding communication between browsers and servers.

Alternatives? Definitely! Libraries like `httplib2`, `treq`, and `aiohttp` are out there, but `requests` stands out for its simplicity.

As for the guts, `requests` uses urllib3 underneath. When you send a request, it constructs an HTTP message, sends it to the server, waits for a response, and provides you with a `response` object packed with useful methods.

## See Also:

- Official Requests documentation (http://docs.python-requests.org/)
- Python 'http' package detailed info (https://docs.python.org/3/library/http.html)
- More on HTTP (https://developer.mozilla.org/en-US/docs/Web/HTTP)
- Alternatives to ‘requests’ (http://docs.python-requests.org/en/latest/community/other-projects/)