---
title:                "Swift recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

As a Swift programmer, you may come across the need to make HTTP requests with basic authentication. This is commonly used when accessing APIs that require authentication, such as social media platforms or web services.

## How To

To send an HTTP request with basic authentication in Swift, you will need to use the `URLSession` and `URLRequest` classes. Here's an example of how to do it:

```
let urlString = "www.example.com/api"
let url = URL(string: urlString)

// Create a URLRequest with the URL
var request = URLRequest(url: url!)

// Set the HTTP method to POST
request.httpMethod = "POST"

// Create a data task with URLSession and pass in the request
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    // Check for errors and handle the response here
}

// Add basic authentication to the request
let username = "your_username"
let password = "your_password"
let authenticationString = "\(username):\(password)"
let data = authenticationString.data(using: .utf8)
let base64String = data!.base64EncodedString(options: Data.Base64EncodingOptions(rawValue: 0))

// Set the value of the Authorization header with the base64 encoding
request.setValue("Basic \(base64String)", forHTTPHeaderField: "Authorization")

// Start the data task
task.resume()
```

The above code will create a request with the given URL and add basic authentication to it before sending it using a data task. You can handle the response in the completion handler of the task and perform any necessary actions or error handling.

## Deep Dive

When adding basic authentication to an HTTP request, the important thing to understand is that the `Authorization` header must be set with a base64-encoded string that includes the username and password in the format `username:password`. This is usually done by combining the username and password with a colon and then converting it to a Data object before encoding it with base64.

It's also worth mentioning that basic authentication is not the most secure way to authenticate requests, as the username and password are easily visible in the code. For more secure authentication, you may want to consider using OAuth or API keys.

## See Also

- [Apple Developer Documentation on URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Article on HTTP Basic Authentication](https://www.redhat.com/en/blog/handing-basic-authentication-using-tools-json)
- [Tutorial on making HTTP requests in Swift](https://www.raywenderlich.com/824-cookies-and-urlsession-in-swift#toc-anchor-004)