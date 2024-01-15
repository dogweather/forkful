---
title:                "Sending an http request with basic authentication"
html_title:           "Swift recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

If you're working with APIs or web services, sending an HTTP request with basic authentication is necessary for authorizing and accessing protected resources. This allows you to securely transmit sensitive information between your application and server.

## How To

To send an HTTP request with basic authentication in Swift, follow these steps:

1. Import the Foundation framework into your project:
```Swift
import Foundation
```

2. Create a `URLRequest` object with the URL of the endpoint you want to access:
```Swift
let url = URL(string: "https://example.com/api/resource")!
var request = URLRequest(url: url)
```

3. Set the HTTP method to `GET` or `POST` depending on what your API requires:
```Swift
request.httpMethod = "GET"
```

4. Add the basic authentication credentials to the request by creating a `Data` object from your username and password, and setting it as the value for the `Authorization` header:
```Swift
let username = "myUsername"
let password = "myPassword"
let credentials = "\(username):\(password)".data(using: .utf8)
if let encodedCredentials = credentials?.base64EncodedString() {
    request.setValue("Basic \(encodedCredentials)", forHTTPHeaderField: "Authorization")
}
```

5. Send the request and handle the response:
```Swift
let task = URLSession.shared.dataTask(with: request) { (data, response, error) in
    if let data = data {
        // Handle the response data
    } else {
        print(error?.localizedDescription ?? "Error sending request")
    }
}
task.resume()
```

## Deep Dive

Sending an HTTP request with basic authentication involves setting the `Authorization` header with a base64-encoded string of your username and password, separated by a colon. It's important to note that basic authentication is not considered secure and should only be used with HTTPS connections to prevent the credentials from being intercepted.

## See Also

- [Apple Documentation on URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [This tutorial on Accessing REST APIs in Swift using URLComponents](https://medium.com/@darthpelo/accessing-rest-apis-in-swift-4ceb8049c038)