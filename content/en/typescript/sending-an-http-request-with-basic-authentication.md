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

## Why

Sending HTTP requests with basic authentication is a common practice in web development to allow secure access to resources or APIs. It ensures that only authorized users can access the requested data.

## How To

To send an HTTP request with basic authentication, we first need to create a HTTP client using the `axios` library. We can specify the authorization header in the `axios` configuration as shown below:

```TypeScript
import axios from 'axios';

const username = 'yourusername';
const password = 'yourpassword';

// Create the HTTP client
const httpClient = axios.create({
  // Specify the base URL for the request
  baseURL: 'https://example.com',
  // Configure the authorization header with the base64 encoded credentials
  auth: {
    username: username,
    password: password
  }
});

// Send the GET request and log the response
httpClient.get('/api/data')
  .then(response => console.log(response.data))
  .catch(error => console.error(error));
```

The `auth` option in the `axios` configuration accepts an object with the `username` and `password` fields. The credentials are then base64 encoded and sent as part of the request headers.

Running the above code example will result in a successful HTTP request with basic authentication. The `response.data` will contain the requested data from the specified URL.

## Deep Dive

Basic authentication is a simple authentication scheme that sends credentials in clear text, making it vulnerable to security threats such as man-in-the-middle attacks. As a result, it is recommended to use it only over HTTPS connections to ensure the credentials are encrypted.

Additionally, basic authentication requires the credentials to be sent with every request, which can be inefficient for applications performing multiple requests. In such cases, it is recommended to use other forms of authentication such as OAuth or token-based authentication.

## See Also

- [axios GitHub Repository](https://github.com/axios/axios)
- [Basic Access Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [HTTP Authentication: Basic and Digest Access Authentication](https://www.ietf.org/rfc/rfc2617.txt)