---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# A Quick Guide to Sending HTTP Requests in TypeScript

## What & Why?
Sending HTTP requests means communicating with a server or external API to retrieve, send, or update data. As a programmer, you'll often do this to fetch data, or to interact with third-party services.

## How to:
In TypeScript, we use the `fetch` approach. Let's consider this example:

```TypeScript 
const url = 'https://jsonplaceholder.typicode.com/posts';
fetch(url)
  .then(response => response.json())
  .then(data => console.log(data));
```

The output will be printed in the console, a list of posts fetched from the placeholder API.

For error handling, we can modify our code to:

```Typescript
fetch(url)
  .then(response => {
    if (!response.ok) {
      throw new Error('HTTP error ' + response.status);
    }
    return response.json();
  })
  .then(data => console.log(data))
  .catch(function () {
    console.log('An error occurred while fetching data');
  });
```

## Deep Dive
Historically, XMLHttpRequest was used for HTTP requests. But over time, `fetch` has largely replaced it due to its more straightforward syntax and the ability to return promises. Alternatively, we can also use the `axios` library, which provides greater functionality than fetch.

The implementation details of sending an HTTP request involve creating an HTTP/HTTPS request with a method (GET, POST, DELETE, etc.), the URL, and optional headers and body data. It's then sent to the server, which responds and this response might be asynchronously handled.

## See Also
For further reading:

- More on Fetch API: [MDN Web Docs Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Comparison of Fetch with Axios: [Fetch vs Axios](https://www.smashingmagazine.com/2020/06/rest-api-fetch-axios/)
- More on XMLHttpRequest: [MDN Web Docs XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- HTTP Methods explained: [RESTful API Methodology](https://restfulapi.net/http-methods/)