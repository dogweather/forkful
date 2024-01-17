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

## What & Why?
Sending an HTTP request with basic authentication is a way for programmers to securely access data from a server that requires authentication. It involves including a username and password in the HTTP request header, allowing the server to verify the identity of the requester and grant access to the requested data.

## How to:
In Swift, sending an HTTP request with basic authentication can be done using the `URLRequest` and `URLSession` APIs. First, create a `URL` with the URL of the server endpoint. Then, create a `URLRequest` with the `url` and set its `httpMethod` to "GET" or "POST". Next, add the basic authentication credentials to the header of the `URLRequest` using the `setValue(_:forHTTPHeaderField:)` method. Finally, use the `URLSession` API to send the request and handle the response.

Example:
```Swift
// Creating the URL and URL Request
let url = URL(string: "https://api.example.com/data")
var request = URLRequest(url: url)
request.httpMethod = "GET"

// Adding basic authentication credentials to the header
let username = "myUsername"
let password = "myPassword"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData?.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// Sending the request and handling the response
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print(error?.localizedDescription ?? "Response Error")
        return
    }
    // Handle the data from the response
    print(String(data: data, encoding: .utf8)!)
}
task.resume()
```

Output:
```
{"id": 123, "name": "John Doe", "age": 35}
```

## Deep Dive:
Sending HTTP requests with basic authentication has been a common practice since the early days of the internet. It was initially used for accessing data from FTP servers, but with the rise of RESTful APIs, it has become a widely used method for accessing data from web servers.

There are other types of authentication supported by HTTP, such as Digest, OAuth, and OAuth2. Each has its own advantages and use cases, but basic authentication remains the simplest method.

To implement basic authentication, the username and password are typically encoded using Base64, a common form of data encoding. However, this does not provide a high level of security as the credentials can easily be decoded by anyone with access to the request header.

## See Also:
- Official documentation for [URLRequest](https://developer.apple.com/documentation/foundation/urlrequest) and [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- A tutorial on [Sending HTTP Requests in Swift](https://programmingwithswift.com/how-to-send-an-http-request-in-swift/)