---
title:                "Python recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
HTTP requests are a fundamental aspect of web development and allow for the communication between a client (such as a web browser) and a server. Understanding how to send HTTP requests using Python is crucial for interacting with web applications and APIs.

## How To

Sending an HTTP request using Python is relatively simple, thanks to the extensive libraries and modules available. The `requests` library is a popular choice for sending HTTP requests, so we will be using that in our examples.

First, we need to import the `requests` library into our code:

```Python
import requests
```

Next, we can use the `get()` method from the `requests` library to send an HTTP GET request to a specified URL. Let's use the OpenWeatherMap API as an example, which allows us to get current weather information for a given location.

```Python
response = requests.get('http://api.openweathermap.org/data/2.5/weather?q=London&appid={API_KEY}')
```

We can also add parameters to our request, such as the location and API key, in the form of a dictionary:

```Python
params = {'q': 'London', 'appid': {API_KEY}}
response = requests.get('http://api.openweathermap.org/data/2.5/weather', params=params)
```

Once we have made our request, we can access the response data using the `json()` method:

```Python
data = response.json()
```

And there we have it! We have successfully sent an HTTP request and received a response from the OpenWeatherMap API.

## Deep Dive

HTTP requests involve a series of steps, including establishing a connection, sending the request message, receiving the response message, and closing the connection. This process is handled by the `requests` library behind the scenes, allowing us to focus on the coding and data manipulation aspects.

One important concept to understand when sending an HTTP request is the use of request methods. The most commonly used methods are `GET`, `POST`, `PUT`, and `DELETE`, each used for a specific purpose. The `GET` method is used to retrieve data from a server, while `POST` is used to send data to a server. `PUT` is used to update existing data, and `DELETE` is used to delete data from a server.

In our example, we used the `GET` method to retrieve data from the OpenWeatherMap API. However, depending on the API or web application being used, different methods may be required.

## See Also
- [The `requests` library documentation](https://docs.python-requests.org/en/master/)
- [HTTP Methods - HTTP | MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Understanding and working with APIs | MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Client-side_web_APIs/Introduction)

Sending HTTP requests is a crucial skill for any programmer working with web applications or APIs. With the `requests` library and a basic understanding of HTTP methods, we can easily incorporate HTTP requests into our Python projects. Happy coding!