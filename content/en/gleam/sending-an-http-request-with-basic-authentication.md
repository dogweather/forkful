---
title:                "Sending an http request with basic authentication"
html_title:           "Gleam recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why 

Sending HTTP requests with basic authentication is essential for accessing and retrieving protected resources from a web server. This type of authentication provides an additional layer of security to prevent unauthorized access to sensitive information.

## How To 

To send an HTTP request with basic authentication in Gleam, you will need to use the `http` and `base64` standard libraries. First, import these libraries at the top of your file:

```
import http
import base64
```

Next, create a `HttpClient` instance and specify the URL you want to send the request to:

```
let client = http.client("https://example.com/my/resource")
```

Then, use the `auth_basic` method to add the basic authentication header to your request. This method takes in a username and password as arguments:

```
let client_with_auth = client |> http.headers([[ "Authorization", http.auth_basic("username", "password") ]])
```

Now, you can make your HTTP request using the `get` or `post` methods:

```
let response = client_with_auth |> http.get()
```

Finally, you can access the response body and status code to see if your request was successful:

```
let body = response |> http.body()
let status_code = response |> http.status()
```

Below is a complete example of sending an HTTP request with basic authentication and accessing the response:

```
import http
import base64

let client = http.client("https://example.com/my/resource") 
let client_with_auth = client |> http.headers([[ "Authorization", http.auth_basic("username", "password") ]])
let response = client_with_auth |> http.get()
let body = response |> http.body()
let status_code = response |> http.status()

## Deep Dive 

When sending an HTTP request with basic authentication, the credentials are encoded in a base64 string and added to the `Authorization` header. This allows the server to verify the identity of the client making the request.

The `http.auth_basic` method handles the encoding of the credentials for you, but it is important to note that this type of authentication is not secure on its own. It is recommended to use HTTPS in conjunction with basic authentication for a more secure transfer of sensitive information.

## See Also 
- Gleam documentation on HTTP requests: https://gleam.run/docs/http
- Standard library for HTTP: https://github.com/lpil/gleam-http
- Standard library for base64 encoding: https://github.com/gleam-lang/gleam_base64