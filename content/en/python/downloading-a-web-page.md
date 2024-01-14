---
title:                "Python recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
Downloading web pages is a common task in web scraping or data mining. It allows developers to extract useful information from websites and use it for various purposes such as market research, data analysis, or content aggregation.

## How To
To download a web page in Python, we will use the `requests` library. First, we need to install it using `pip install requests` in our terminal. Then, we can import it in our code:

```Python
import requests
```

Next, we will use the `get()` method and pass in the URL of the web page we want to download. Let's download the home page of Google and store the response in a variable called `response`:

```Python
response = requests.get("https://www.google.com/")
```

Now, we can access the content of the web page using the `text` property of the `response` object:

```Python
print(response.text)
```

If we run this code, we will see the HTML code of the Google home page printed in our terminal. We can also save the content of the page in a file using the `write()` method:

```Python
with open("google.html", "w") as file:
    file.write(response.text)
```

This will create a new file called `google.html` and save the web page content in it. We can then open the file and see the HTML code in a text editor or browser.

## Deep Dive
Behind the scenes, the `requests` library sends an HTTP request to the web server and receives an HTTP response. The `get()` method sends a GET request which is the most common type of request used for downloading web pages. By default, the library will follow redirects and handle errors for us.

We can also customize our request by passing additional parameters to the `get()` method. For example, we can add headers, cookies or authentication credentials. We can also specify a timeout to avoid getting stuck in case the web server is not responding.

In addition to the HTML content, the `response` object also contains other useful information such as the status code, headers, and cookies. We can access them using the respective properties of the `response` object.

## See Also
Here are some helpful links for further reading about downloading web pages in Python:

- [Requests library documentation](https://requests.readthedocs.io/en/master/)
- [HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [HTML tutorial](https://www.w3schools.com/html/)