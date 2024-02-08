---
title:                "Sending an HTTP request with basic authentication"
aliases:
- en/swift/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:40.647124-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication involves attaching a username and password to a request for gated web content. Programmers do this to access APIs or resources that are restricted to authorized users.

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
