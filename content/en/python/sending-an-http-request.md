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

## Why

Sending HTTP requests is a crucial part of web development and allows users to communicate with servers and fetch data from websites. It is necessary for tasks such as retrieving information, making API calls, and interacting with web forms.

## How To

To send an HTTP request in Python, we will be using the built-in `requests` module. First, we need to import the module:

```Python
import requests
```

Next, we can use the `get()` method from the `requests` module to make a GET request to a specified URL:

```Python
response = requests.get("https://www.example.com")
```

We can also include parameters in our request:

```Python
params = {"key": "value"}
response = requests.get("https://www.example.com", params=params)
```

To make a POST request, we can use the `post()` method and pass in any necessary data:

```Python
data = {"key": "value"}
response = requests.post("https://www.example.com", data=data)
```

After making the request, we can access the response using the `response` variable. We can view the HTML of the page using the `text` attribute:

```Python
print(response.text)
```

We can also view the status code of the response using the `status_code` attribute:

```Python
print(response.status_code)
```

## Deep Dive

The `requests` module makes it easy for us to send HTTP requests in Python by handling all of the underlying complexities. It also allows us to customize our requests by adding headers, authentication, and other options.

The GET and POST methods are just two of the many available request methods, such as PUT, DELETE, and PATCH. The `requests` module also has a built-in JSON decoder, making it easy to work with JSON data.

It is important to note that when making multiple requests, it is best to use the `Session` object from the `requests` module. This allows for more efficient use of network resources and improved performance.

To learn more about the `requests` module and its capabilities, check out the official documentation [here](https://requests.readthedocs.io/en/latest/).

## See Also

For more information on web development in Python, check out these helpful resources:

- [Official Python Documentation](https://docs.python.org/3/library/http.html)
- [Flask - A Lightweight Web Framework](https://flask.palletsprojects.com/en/2.0.x/)
- [Django - A Full-Featured Web Framework](https://www.djangoproject.com/)