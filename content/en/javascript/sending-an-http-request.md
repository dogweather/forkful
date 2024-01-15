---
title:                "Sending an http request"
html_title:           "Javascript recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request is a fundamental task in web development. It allows your application to communicate with servers and retrieve data, making it necessary for any type of dynamic web application or website.

## How To

Sending an HTTP request involves using the `fetch()` function in Javascript. This function takes in a URL as a parameter and returns a `Promise` which can be used to access the response data. Here's an example:

```
Javascript
fetch('http://www.example.com')
  .then(response => response.text())
  .then(data => console.log(data));
```

In this example, we are sending a `GET` request to the URL `http://www.example.com` and accessing the response as plain text. We can also specify other types of data, such as `JSON` or `blob`, depending on the type of response we expect to receive.

## Deep Dive

Apart from the `fetch()` function, there are other ways to send an HTTP request in Javascript. One alternative is using the `XMLHttpRequest` object, which has been around for a long time and has been the go-to method for sending requests in older browsers. However, with the introduction of `fetch()`, it has become the preferred method due to its simplicity and built-in support for promises.

When sending a request, it's essential to handle any potential errors that may occur. For this, we use the `catch()` method to catch any errors that may arise during the request. It's also a good practice to set headers for your request, which can be done using the `Headers` object.

## See Also

- [MDN Web Docs: fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- [MDN Web Docs: XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [MDN Web Docs: Headers](https://developer.mozilla.org/en-US/docs/Web/API/Headers)