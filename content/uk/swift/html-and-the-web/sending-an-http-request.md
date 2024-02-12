---
title:                "Надсилання HTTP-запиту"
aliases:
- /uk/swift/sending-an-http-request.md
date:                  2024-01-20T18:00:45.550818-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Sending an HTTP request lets your app talk to a web server—like asking for data or sending it. Programmers use this to interact with web APIs, grabbing fresh info or pushing changes.

## How to: (Як це зробити:)
```Swift
import Foundation

let url = URL(string: "https://api.example.com/data")!
var request = URLRequest(url: url)
request.httpMethod = "GET"

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data, 
              let string = String(data: data, encoding: .utf8) {
        print(string)
    }
}

task.resume()
```
Output:
```
{"data": "some value"}
```

## Deep Dive (Поглиблене занурення)
Sending HTTP requests is a basic yet powerful way to interact with the web. It began with simple web pages, evolved with the internet, and now it's essential for modern apps. Compared to other methods like WebSocket, HTTP requests are stateless and good for standalone operations. Underneath, they work by establishing a TCP connection, sending plain-text commands (GET, POST, etc.), and listening for a response.

## See Also (Дивіться також)
- Apple's URLSession documentation: [URLSession | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- Swift's official HTTP networking guide: [Networking Overview | Swift.org](https://www.swift.org/documentation/api-design-guidelines#networking)
- An article on RESTful APIs: [Understanding RESTful APIs](https://restfulapi.net/)
