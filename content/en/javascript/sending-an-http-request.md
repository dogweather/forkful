---
title:                "Javascript recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

One of the fundamental actions of a web application is making HTTP requests. This is because it allows the application to communicate with other servers and retrieve data or perform actions. Without this ability, our web applications would not be able to function properly.

## How To

Sending an HTTP request in javascript can be done using the built-in `fetch()` function. This function accepts a URL as an argument and returns a promise which resolves to a response object. Let's take a look at an example:

```Javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(data => console.log(data))
```

In this example, the `fetch()` function is used to make an HTTP GET request to the specified URL. The first `.then()` block takes the response object and turns it into a JSON format, while the second `.then()` block logs the data retrieved from the request to the console.

The output of this request will be an object containing the data from the first post on the JSONPlaceholder API:

```Javascript
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat",
  "body": "quia et suscipit\nsuscipit recusandae"
}
```

Other HTTP methods such as POST, PUT, and DELETE can also be used with the `fetch()` function by passing in additional parameters as the second argument. For example, to send a POST request with some data, we can do the following:

```Javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  body: JSON.stringify({
    title: 'New Post',
    body: 'This is a new post',
    userId: 1
  }),
  headers: {
    'Content-type': 'application/json; charset=UTF-8'
  }
})
  .then(response => response.json())
  .then(data => console.log(data))
```

This code will send a POST request to the specified URL with the data provided in the `body` parameter. The `headers` parameter is used to specify the content type, in this case, it is `application/json`, so the data sent in the body needs to be converted to JSON format using the `JSON.stringify()` function.

## Deep Dive

There are several important things to keep in mind when sending HTTP requests in javascript. First, the `fetch()` function returns a promise, meaning that we can use `.then()` and `.catch()` to handle the response or any errors. If we want to use async/await syntax, we can also use the `await` keyword in front of the `fetch()` function.

Second, we need to be aware of the Same-Origin Policy, which is a security measure that restricts javascript from making requests to a different origin (domain) than the one it was loaded from. This means that if the web application is loaded from `domainA.com`, it can only make requests to `domainA.com` and not `domainB.com`.

Lastly, we can also add other options such as setting a timeout for the request or adding custom headers to the `fetch()` function. It is important to understand these options and use them properly to handle different scenarios when making HTTP requests.

## See Also

- [MDN web docs - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Using Fetch - A Beginner's Guide](https://www.taniarascia.com/how-to-use-the-javascript-fetch-api-to-get-data/)
- [HTTP request methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)