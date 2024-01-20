---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending HTTP requests is a way for your JavaScript code to communicate with servers and fetch data. It's a core part of many apps' functionality, enabling user interactivity and dynamic content updates.

## How To:

Here's how you can send an HTTP GET request using fetch() API in JavaScript.

```Javascript
fetch('https://api.github.com/users/github')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```

In this example, we are fetching data from Github's API for the 'github' user. After the data is fetched, it is logged to the console. If there's an error executing the request, it is caught and logged in the catch block.

## Deep Dive

Historically, we used to leverage XMLHttpRequest for sending HTTP requests. But this has been simplified and improved with the Fetch API, which returns Promises and is more powerful and flexible.

An alternative to `fetch` is the `axios` library. It's a popular choice since it provides some additional features like interceptors and automatic transforms, and it's more browser-compatible than fetch.

Bear in mind that when you're sending a HTTP request, you're actually sending a packet of data over the Internet to a specific server. This packet includes information about what you're requesting (GET, POST, etc.), where you're sending the request, and any additional data (like headers or body content). The server then processes this request and sends a response back to the client.

## See Also

[MDN Documentation on Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) \
[Axios Library](https://axios-http.com/docs/intro) \
[MDN Documentation on XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)