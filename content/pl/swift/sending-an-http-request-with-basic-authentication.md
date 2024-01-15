---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Swift: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Sending an HTTP request with basic authentication allows for secure communication between a client and a server. This is especially important when sensitive data, such as personal information, is being transferred.

## Jak to zrobić

```Swift
let username = "john_doe"
let password = "secretpassword"

// Encode the username and password in base64
let authString = "\(username):\(password)".data(using: .utf8)?.base64EncodedString()

// Create the HTTP request
guard let requestURL = URL(string: "http://www.example.com/login") else {
  print("Invalid URL")
  return
}

var request = URLRequest(url: requestURL)

// Add the basic authentication header
request.setValue("Basic \(authString)", forHTTPHeaderField: "Authorization")

// Execute the request
let task = URLSession.shared.dataTask(with: request) { data, response, error in
  if let error = error {
    print("Error: \(error)")
  } else if let data = data {
    // Handle the response data
    print("Response: \(data)")
  }
}

task.resume()
```

Przykładowe wyjście:

```Swift
Response: Optional([60, 104, 116, 109, 108, 62, 10, 32, 32, 60, 104, 101, 97, 100, 62, 10, 32, 32, 32, 32, 60, 116, 105, 116, 108, 101, 62, 82, 101, 113, 117, 101, 115, 116, 32, 83, 117, 99, 99, 101, 115, 115, 60, 47, 116, 105, 116, 108, 101, 62, 10, 32, 32, 60, 47, 104, 101, 97, 100, 62, 10, 32, 32, 60, 98, 111, 100, 121, 62, 10, 32, 32, 32, 32, 60, 104, 49, 62, 76, 111, 103, 105, 110, 32, 115, 117, 99, 99, 101, 115, 115, 102, 117, 108, 108, 121, 32, 108, 111, 103, 103, 101, 100, 32, 105, 110, 32, 115, 117, 99, 99, 101, 115, 115, 108, 51, 54, 122, 112, 121, 74, 60, 47, 104, 49, 62, 10, 32, 32, 32, 32, 60, 104, 50, 62, 87, 101, 108, 99, 111, 109, 101, 32, 111, 110, 32, 98, 111, 97, 114, 100, 44, 32, 74, 111, 104, 110, 33, 60, 47, 104, 50, 62, 10, 32, 32, 60, 47, 98, 111, 100, 121, 62, 10, 60, 47, 104, 116, 109, 108, 62])
```

## Deep Dive

When sending an HTTP request with basic authentication, the username and password are encoded in base64 and added to the header of the request as a string in the format "username:password". This ensures that the sensitive information is not transmitted in plain text and can only be accessed by the intended recipient.

## See Also

- [Working with HTTP requests in Swift](https://www.hackingwithswift.com/articles/118/working-with-http-requests-in-swift)
- [Using URLSession to make web requests in Swift](https://www.avanderlee.com/swift/nsurlsession-networking/)