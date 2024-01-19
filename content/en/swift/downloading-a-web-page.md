---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage refers to the process of retrieving all the data it contains from the server it resides on. This is common during web scraping, a practice used by programmers to automatically extract large amounts of data from the web.

## How to:

Here's a simple example using the URLSession in Swift to perform a data task (HTTP GET request):

```Swift
import Foundation

let url = URL(string: "https://your-site.xyz")!

let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str!)\n\n")
    }
}

task.resume()
```

After you've made requests to an URL, you'll receive raw HTML data which can be converted into a string and printed.

## Deep Dive:

Historically, downloading a webpage was a bit more intensive. You'd have to open a socket, point it at the web server, and manually send HTTP commands. URLSession, introduced in iOS 7, simplified this by encapsulating the entire HTTP request/response process.

There are other alternatives such as Alamofire library, which provides a more user-friendly interface for server interaction. Additionally, SwiftNIO offers a more extensive set of tools for network programming.

When downloading a webpage, it's worth noting it's a network-bound task. It might take some time. It's advisable to perform this on a background thread to avoid stalling the interface. The URLSession data task will handle this for you.

## See Also:

1. Designated Initializers vs Convenience Initializers in Swift - [Link](https://docs.swift.org/swift-book/LanguageGuide/Initialization.html)
2. More about URLSession and Swift Network Programming - [Link](https://developer.apple.com/documentation/foundation/urlsession)
3. SwiftNIO, a cross-platform asynchronous event-driven network application framework - [Link](https://swift.org/swift-nio/)
4. Alamofire, an HTTP networking library written in Swift - [Link](https://github.com/Alamofire/Alamofire)