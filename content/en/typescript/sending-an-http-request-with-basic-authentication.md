---
title:                "Sending an http request with basic authentication"
html_title:           "TypeScript recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication is a common practice among programmers when working with web-based APIs that require user authentication. This involves sending a username and password along with the request in order to access protected resources. Basic authentication provides a simple yet effective method for verifying the identity of the requesting user and determining what actions they are authorized to perform.

## How to:
```TypeScript
import axios from 'axios';

const URL = 'https://api.example.com/resource'; // replace with desired API endpoint
const username = 'myUsername'; // replace with actual username
const password = 'myPassword'; // replace with actual password

// create a Basic Auth header with the username and password encoded in base64
const basicAuthHeader = `Basic ${btoa(username + ':' + password)}`;

// make a GET request to the API endpoint with the Basic Auth header 
axios.get(URL, { headers: { Authorization: basicAuthHeader } })
  .then(response => {
    // handle successful response
    console.log(response.data);
  })
  .catch(error => {
    // handle error
    console.log(error);
  });
```

Sample output after successful authentication:
```TypeScript
{ 
  id: 12345,
  username: 'myUsername',
  email: 'myemail@example.com',
  role: 'admin' 
}
```

## Deep Dive:
Basic authentication has been around since the early days of the internet and is still widely used today. It operates on the concept of sending credentials in plain text, which makes it vulnerable to security risks such as man-in-the-middle attacks. As a result, many developers have turned to more secure alternatives such as OAuth. However, basic authentication is still a convenient option for simple and low-risk APIs.

In order to implement basic authentication in TypeScript, we use a combination of the `btoa()` function, which encodes a string to base64, and the `Authorization` header in our HTTP request. It is important to note that different web frameworks and APIs may have specific requirements for the format of the basic auth header, so be sure to consult their documentation for proper implementation.

## See Also:
- [Axios: Making HTTP Requests in TypeScript](https://blog.logrocket.com/axios-or-fetch-api/)
- [Basic Authentication vs. OAuth: Which is Better?](https://blog.apiary.io/2016/03/10/choosing-an-api-authentication-method/)
- [Security Risks of Basic Authentication](https://www.owasp.org/index.php/Basic_Authentication)