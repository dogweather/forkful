---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a mechanism for transferring data between webpages and servers with user credentials. Programmers opt for this because it's handy for server-side rendering, accessing APIs, and interacting with external databases.

## How to:

In Swift, with the `URLSession` class, we construct and send HTTP requests. Here's a clean way of implementing an HTTP request with basic authentication.

```Swift
import Foundation

let url = URL(string: "https://example.com")!

var urlRequest = URLRequest(url: url)
urlRequest.httpMethod = "GET"

// Prepare your credentials and convert them to base64 
let username = "testUser"
let password = "testPassword"
let loginInfo = "\(username):\(password)".data(using: .utf8)?.base64EncodedString()

// Add credentials in header
urlRequest.addValue("Basic \(loginInfo ?? "")", forHTTPHeaderField: "Authorization")

// Execute the request
URLSession.shared.dataTask(with: urlRequest){ (data, response, error) in
    // Handle the response here
}.resume()
```

The response you'll receive is based on the data you're requesting from the server.

## Deep Dive

Historically, using HTTP requests with basic authentication has been handled with libraries like Alamofire. But in SwiftUI, with powerful native capabilities, we can handle HTTP requests without external dependencies.

An alternative to basic authentication would be bearer authentication, where single-use tokens are issued for user authentication. 

The implementation of HTTP requests with basic authentication in Swift uses the `URLSession` class. It encapsulates a shared singleton session for basic HTTP GET requests. The base64 encoding of the username and password provides basic HTTP authentication. 

## See Also:

- URLSession Documentation: https://developer.apple.com/documentation/foundation/urlsession
- Basic HTTP Auth: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Bearer Auth Alternative: https://oauth.net/2/bearer-tokens/
- Alamofire Library: https://github.com/Alamofire/Alamofire