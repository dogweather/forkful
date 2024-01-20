---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Python HTTP Requests: A Simple Guide

## What & Why?

Sending an HTTP request is primarily about getting data from or sending data to a server. Programmers use it to interact with web services, fetch web pages, or even to update data on a web server.

## How to:

Python offers a few libraries to send HTTP requests, but for sheer simplicity, let's use 'requests' library.

First, install the library using pip if you haven't already.

```python
pip install requests
```

Now let's issue a GET request.

```python
import requests

response = requests.get('http://httpbin.org/get')

print(response.content)
```

You simply pass the URL you want to make a request to the GET function, and it will return a response. This output is just the raw HTML of the page.

## Deep Dive

Understanding HTTP requests goes back to the early stages of the Web. In the late 1990s, Roy Fielding, part of the team that wrote the HTTP specification, defined an architectural style of networked systems called REST. Now, most web services use RESTful APIs, thus the need to understand HTTP requests.

Aside from the requests library, Python also provides other modules like httplib2, treq, and http.client for sending HTTP requests. Each has its own use-cases, strengths, and limitations.

Behind the scenes, when you send an HTTP request, your client (the device you're using) sends a message to the server with request headers (to provide metadata about the kind of request being made) and sometimes a body (which carries the actual request data, if applicable).

## See Also

To delve deeper into Python's capability of managing HTTP requests, check out these resources:

1. Python's requests library documentation: [Docs](https://requests.readthedocs.io/en/master/)
2. http.client — HTTP protocol client: [Docs](https://docs.python.org/3/library/http.client.html)
3. A deep dive into HTTP Requests with Python: [Real Python](https://realpython.com/python-requests/)  
   
Remember, the way to mastery is practice and research. So have fun experimenting and learning more about HTTP requests, and happy coding!