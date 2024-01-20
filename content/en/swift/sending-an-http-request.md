---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

---

## What & Why?

Sending an HTTP request is a way for your program to get or send info to a server. Programmers do this to get data, send data, or interact with APIs.

---
## How to:
In Swift, use the URLSession library to send HTTP requests. Here's how to GET some JSON:

```Swift
import Foundation

let url = URL(string: "https://api.github.com/users/octocat")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print(str) // JSON output
    }
}
task.resume()
```
This prints the Github profile of 'octocat' in JSON. Sending a POST request? Use URLRequest.

```Swift
import Foundation

let url = URL(string: "https://httpbin.org/post")!
var request = URLRequest(url: url)
request.httpMethod = "POST"
request.httpBody = "foo=bar&baz=qux".data(using: .utf8)

let task = URLSession.shared.dataTask(with: request) {(data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print(str) // JSON output
    }
}
task.resume()
```

The output shows your POST data echoed back as JSON.

---

## Deep Dive

HTTP requests started with the internet. In 1996, HTTP/1.0 turned into HTTP/1.1, and POST was born. Swift came on the scene in 2014, and its native networking library URLSession has been sending HTTP requests since.

Libraries like AlamoFire offer detailed network operations. But URLSession is enough for many tasks.

When you call `dataTask(with:completionHandler:)`, URLSession creates a task but keeps it idle. The task fires only when you call `resume()`. Don't forget it or nothing happens.

---

## See Also

- [URLSession - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP/1.1: Method Definitions](https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html)
- [Alamofire Github](https://github.com/Alamofire/Alamofire)