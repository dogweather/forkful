---
title:                "Swift recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
HTTP requests are an essential part of building any modern application that needs to communicate with a server. Whether you're building a social media app or a shopping platform, sending HTTP requests allows your app to access and retrieve data from the server. In this blog post, we'll explore the basics of sending an HTTP request in Swift.

## How To
To send an HTTP request in Swift, we'll be using the built-in URLSession class. This class allows us to create and manage URL requests, as well as handle the response from the server.

First, we need to create a URL object from the server's address. This can be done using the URL initializer, passing in the string representation of the URL. Then, we create a request using this URL and specify the HTTP method (GET, POST, PUT, etc.).

```
Swift
// Create URL object
let url = URL(string: "https://example.com/api/users")

// Create request with HTTP method
var request = URLRequest(url: url!)
request.httpMethod = "GET"
```

Next, we can add any necessary headers to the request, such as API keys or authentication tokens, using the `addValue(_:forHTTPHeaderField:)` method.

```
Swift
// Add header to request
request.addValue("12345", forHTTPHeaderField: "API-Key")
```

Once our request is set up, we can use the `shared` instance of URLSession to send it. The `dataTask(with:completionHandler:)` method will handle the request and provide us with a response in the completion handler.

```
Swift
// Create URLSession
let session = URLSession.shared

// Send request and handle response
session.dataTask(with: request) { data, response, error in
  // Handle response here
}.resume()
```

In the completion handler, we can handle the response by checking for any errors and retrieving the data if the request was successful.

```
Swift
// Handle response
if let error = error {
  // Handle error
} else if let data = data {
  // Process data
  let output = String(data: data, encoding: .utf8)
  print(output!)
}
```

In this example, we've converted the response data to a string and printed it out, but you can process the data in any way necessary for your app.

## Deep Dive
Sending an HTTP request involves many different components, such as the URL, request headers, and response handling. It's important to understand all of these components to effectively send and handle the response from the server.

Another important aspect to consider is error handling. In the above example, we simply checked for any errors in the completion handler. However, it's important to handle errors properly in case the request fails.

In addition, there are different types of HTTP requests, such as GET, POST, PUT, and DELETE. Each type has its own purpose and implementation, and understanding when to use each one is crucial.

## See Also
- [Apple Developer Documentation on URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Methods - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [Handling Errors in Swift - Swift by Sundell](https://swiftbysundell.com/articles/handling-errors-in-swift/)

Sending an HTTP request in Swift may seem daunting at first, but with the proper understanding and knowledge, it can become a seamless part of your app development process. So go ahead and try implementing this in your next project!