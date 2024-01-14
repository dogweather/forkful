---
title:                "TypeScript recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is an essential part of modern web development. It allows us to communicate with external servers, retrieve data, and make updates to our applications. Whether you're building a simple website or a complex web application, understanding how to send HTTP requests is a valuable skill to have.

## How To

To get started with sending HTTP requests in TypeScript, we first need to install a package that allows us to make these requests. One popular package is `axios`, which is a promise-based library that works both in the browser and Node.js environments. To install `axios`, we can use the following command:

```TypeScript
npm install axios
```

Once `axios` is installed, we can import it into our TypeScript file and use it to send HTTP requests. Let's take a look at an example of how we can send a `GET` request to retrieve data from an external API:

```TypeScript
import axios from 'axios';

axios.get('https://jsonplaceholder.typicode.com/posts/1').then(response => {
    console.log(response.data);
}).catch(error => {
    console.log(error);
});
```

In the above code, we are using the `get()` method from `axios` to make a `GET` request to the specified URL. The `then()` method is used to handle the response and the `catch()` method is used to handle any errors that may occur.

We can also specify additional parameters in our request, such as headers or request data. Here is an example of how we can make a `POST` request with some data:

```TypeScript
axios.post('https://jsonplaceholder.typicode.com/posts', {
    title: 'My New Post',
    body: 'This is the content of my new post',
    userId: 1
}).then(response => {
    console.log(response.data);
}).catch(error => {
    console.log(error);
});
```

In the above code, we are using the `post()` method and passing in the URL and data as parameters. The `data` property in the response object will contain the data that was sent in the request.

## Deep Dive

Behind the scenes, when we make an HTTP request, we are actually making a request to a server using a specific protocol, which is the Hypertext Transfer Protocol (HTTP). The server then processes our request and sends back a response, which usually contains some sort of data or information.

There are different types of HTTP requests that we can make, such as `GET`, `POST`, `PUT`, `PATCH`, and `DELETE`. Each of these requests serves a different purpose and can be used to retrieve or update data on the server.

Additionally, we can also specify headers in our requests, which are used to provide extra information or instructions to the server. For example, we can specify the content type of our data or include authentication tokens in our request headers.

## See Also

- [Official Axios Documentation](https://github.com/axios/axios)
- [HTTP Request Methods](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [HTTP Response Status Codes](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)

Sending HTTP requests is a fundamental concept in web development, and understanding how to do it in TypeScript can greatly enhance our applications. By using packages such as `axios`, we can easily make and handle these requests within our code. With this knowledge, we can now confidently communicate with external servers and make our applications even more dynamic and interactive.