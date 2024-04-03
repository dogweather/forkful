---
date: 2024-01-20 18:00:45.550818-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) ."
lastmod: '2024-03-13T22:44:49.914040-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

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
