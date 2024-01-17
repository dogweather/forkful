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

## What & Why?

Sending an HTTP request is a way for web applications to communicate with servers. Essentially, it is a request sent to a server asking for information or actions to be performed. Programmers use this feature to make web applications interactive, dynamic, and to provide a way for users to access data from a server.

## How to:

Sending an HTTP request in Javascript is a simple process. First, we need to create an instance of the XMLHttpRequest object. This object allows us to send the request and handle the response from the server. Let's see an example:

```
const request = new XMLHttpRequest();
```

Next, we need to specify the URL we want to send the request to, along with the HTTP method we want to use. For example, we can use the `open()` method to set the method and URL:

```
request.open("GET", "https://example.com");
```

Then, we can send the request using the `send()` method:

```
request.send();
```

Now, we can handle the response from the server. We can set a function to be called when the server responds using the `onreadystatechange` event:

```
request.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(this.responseText); // display the response from the server
  }
}
```

## Deep Dive:

HTTP (Hypertext Transfer Protocol) was developed by Tim Berners-Lee in the early 1990s as a way to transfer data between a web server and a web browser. Before the implementation of HTTP, communication between servers and clients was limited and mostly manual.

There are other ways to send requests in Javascript, such as using the `fetch` API or libraries like Axios. These methods typically use promises to handle the response instead of the `onreadystatechange` event.

When sending an HTTP request, there are multiple methods that can be used, such as GET, POST, PUT, DELETE, etc. Each has its own purpose and restrictions, and it is essential to choose the appropriate one based on the desired outcome.

## See Also:

To learn more about sending HTTP requests in Javascript, check out these resources:

- [MDN Web Docs - XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Axios Github Page](https://github.com/axios/axios)