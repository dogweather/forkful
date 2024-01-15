---
title:                "Sending an http request"
html_title:           "TypeScript recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why 

Sending HTTP requests is an essential task in modern web development. It allows interaction with web servers, enabling data transfer, resources retrieval, and much more.

## How To

Sending an HTTP request using TypeScript is made easy with the built-in `fetch` API. Here's a simple example: 

```typescript 
fetch('https://example.com/api/users')
  .then(response => response.json())
  .then(data => console.log(data))
```

When the above code is executed, it will send a GET request to the specified URL and log the returned data to the console. The `fetch` function returns a `Promise` object, which can be resolved to access the response and use the returned data.

To send a POST request with data, the `fetch` function can be used in a slightly different way:

```typescript 
fetch('https://example.com/api/users', {
  method: 'POST',
  body: JSON.stringify({ name: 'John', age: 30 })
})
  .then(response => response.json())
  .then(data => console.log(data))
```

In this example, we are specifying the request method as POST and passing in the data as a JSON string in the `body` option. The server can then handle the data and return a response accordingly.

## Deep Dive

The `fetch` function is just one way of sending HTTP requests in TypeScript. There are other popular libraries and frameworks like Axios, SuperAgent, and Angular's HttpClient, which offer more advanced features and options.

Additionally, HTTP requests can have headers, which provide additional information about the request or response. These can be set using the `headers` option in the `fetch` function or specific methods provided by other libraries.

It is also important to handle errors when making HTTP requests. Most libraries and frameworks allow for error handling through the use of `catch` blocks or error callbacks.

## See Also

- [TypeScript documentation on fetch](https://www.typescriptlang.org/docs/handbook/declaration-files/global-fetch.html)
- [Axios documentation](https://github.com/axios/axios)
- [SuperAgent documentation](https://github.com/visionmedia/superagent)
- [Angular HttpClient documentation](https://angular.io/guide/http)