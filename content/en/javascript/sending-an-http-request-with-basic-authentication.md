---
title:                "Javascript recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
Sending HTTP requests with basic authentication is an important skill to have as a Javascript programmer. It allows you to securely communicate with APIs and retrieve data from remote servers. 

## How To
To send an HTTP request with basic authentication in Javascript, we need to use the `XMLHttpRequest` object. This object allows us to make HTTP requests from our client-side code. First, we need to create a new instance of `XMLHttpRequest` using the `new` keyword:

```javascript
let xhr = new XMLHttpRequest();
```

Next, we need to open the request using the `open()` method. This method takes in three parameters: the HTTP method (e.g. `GET`, `POST`), the URL we want to send the request to, and a boolean value that specifies whether the request should be asynchronous or not. 

```javascript
xhr.open('GET', 'https://example.com/api/users', true);
```

After that, we can set the basic authentication credentials using the `setRequestHeader()` method. This method takes in two parameters: the header name (in this case, `Authorization`) and the basic authentication string which is encoded using Base64.

```javascript
xhr.setRequestHeader('Authorization', 'Basic YWRtaW46cGFzc3dvcmQ=');
```

Finally, we need to send the request and handle the response using the `onreadystatechange` event:

```javascript
xhr.onreadystatechange = function() {
  // Check if the request is complete and the status is 200 (OK)
  if (xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) {
    // The response will be in the `responseText` property
    console.log(xhr.responseText);
  }
};
// Send the request
xhr.send();
```

The output of this code will be the response from the API, which can be used to retrieve and manipulate data as needed.

## Deep Dive
Basic authentication is a simple and widely-used method of password protection for HTTP requests. It is based on the `Authorization` header, which takes in a string of the format `username:password` and encodes it using Base64. It is important to note that although this method can provide some security, it is not recommended for use in production environments as the credentials can still be easily decoded.

To enhance the security of HTTP requests, it is recommended to use other forms of authentication such as OAuth or JSON Web Tokens (JWTs). These methods provide more robust security measures and are widely used in modern web development.

## See Also
- [MDN Web Docs - XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Base64 Encoding Tool](https://www.base64encode.org/)
- [Examples of HTTP Basic Authentication](https://gist.github.com/erikeldridge/bb99e4b7f7b4ceb90bb66ba7850b6b0a)