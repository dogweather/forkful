---
title:                "Sending an http request"
html_title:           "Swift recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is the process of your application communicating with a server over the internet, typically to retrieve or send data. Programmers use this method to seamlessly exchange information between a client (your app) and a server.

## How to:
Sending an HTTP request is a straightforward process in Swift. First, import the `Foundation` framework and create a `URL` object with the desired endpoint. Then, use the `URLSession` class to create a data task with the request and handle the response in a completion handler. Here's a simple example of sending a GET request and retrieving the response data in a string format:

```Swift
import Foundation

let url = URL(string: "https://example.com")!
let request = URLRequest(url: url)

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let data = data {
        let responseString = String(data: data, encoding: .utf8)
        print(responseString)
    }
}

task.resume()
```
Output: The response data in string format.

## Deep Dive:
Sending HTTP requests has been a staple of web development since the creation of the internet. Alternatives to this method include accessing a server's data directly through a database or using a third-party API. However, sending HTTP requests remains the most common and user-friendly approach for exchanging data between a client and server. Under the hood, Swift uses the `URLSession` API to handle the details of the request, including sending the request, receiving the response, and handling errors. Additionally, programmers can customize the request and response by setting specific headers or using different methods, such as POST or PUT.

## See Also:
To learn more about sending HTTP requests in Swift, check out the official documentation for `URLSession` and `URLRequest`. Other helpful resources include tutorials for using the `Alamofire` library for a more streamlined approach to HTTP requests and understanding the principles of the HTTP protocol.