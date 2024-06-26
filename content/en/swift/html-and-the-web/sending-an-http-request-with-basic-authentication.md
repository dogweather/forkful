---
date: 2024-01-20 18:02:40.647124-07:00
description: 'How to: Here''s how to send an HTTP request with basic auth in Swift.'
lastmod: '2024-03-13T22:45:00.396214-06:00'
model: gpt-4-1106-preview
summary: Here's how to send an HTTP request with basic auth in Swift.
title: Sending an HTTP request with basic authentication
weight: 45
---

## How to:
Here's how to send an HTTP request with basic auth in Swift:

```Swift
import Foundation

// Your API endpoint
let url = URL(string: "https://example.com/api/data")!

// Your credentials
let username = "user"
let password = "password"

// Create login data and convert to base64 string
let loginData = String(format: "%@:%@", username, password).data(using: String.Encoding.utf8)!
let base64LoginData = loginData.base64EncodedString()

// Create the request
var request = URLRequest(url: url)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginData)", forHTTPHeaderField: "Authorization")

// Send the request
let session = URLSession.shared
let dataTask = session.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)") // Handle error
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print("Response: \(string)") // Handle response
    }
}

dataTask.resume()
```

Output should be the data returned from the API, or an error message if something goes wrong.

## Deep Dive
Back in the early web days, basic authentication was a quick way to secure resources. Its simplicity made it widely adopted despite being less secure than modern alternatives like OAuth because credentials are not encrypted, just encoded.

Aside from basic auth, alternatives include digest authentication, API keys, OAuth, or JWT (JSON Web Tokens). Each comes with pros and cons around security, ease of use, and the level of protection offered.

When sending an HTTP request with basic auth, it's best practice to ensure you're using HTTPS, so your encoded credentials are securely transmitted. Also, avoid hardcoding credentials; instead, use environment variables or secure vaults.

## See Also
- [Apple's URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP Basic Auth RFC](https://tools.ietf.org/html/rfc7617)
- [OAuth 2.0](https://oauth.net/2/)
