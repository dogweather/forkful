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

## Why
Sending HTTP requests is essential for any programmer who wants to communicate with web servers and retrieve data. In today's digital age, it is crucial to understand how to send HTTP requests to create dynamic and interactive web applications.

## How To
To send an HTTP request in Swift, follow these simple steps:

1. Create a `URL` object using the desired URL.
2. Create a `URLRequest` object with the `URL` object.
3. Set the request method and any necessary parameters using the `httpMethod` and `httpBody` properties.
4. Create an `URLSession` object and call `dataTask` with the `URLRequest` object as a parameter.
5. Use the `resume` method to start the request.
6. Handle the response in the `dataTask`'s completion handler using the `data` and `response` parameters.

Here's an example of sending a GET request to retrieve data from a REST API:

```Swift
let urlString = "https://example.com/api/users"
if let url = URL(string: urlString) {
    var request = URLRequest(url: url)
    request.httpMethod = "GET"
    
    let session = URLSession.shared
    let task = session.dataTask(with: request) { data, response, error in
        if let error = error {
            print("Error: \(error.localizedDescription)")
        } else if let data = data,
            let response = response as? HTTPURLResponse,
            response.statusCode == 200 {
            // Handle data
            print(data)
        }
    }
    task.resume()
}
```
Output:
`Data(bytes: ...)` // Data object containing the retrieved data

## Deep Dive
Sending an HTTP request involves a series of steps that occur behind the scenes. Once the `resume` method is called, the `URLRequest` is converted into raw bytes and sent to the designated URL. The web server receives the request and processes it, returning a response, which includes a status code, headers, and potentially data.

Some common HTTP methods include GET, POST, PUT, and DELETE, which correspond to retrieving, creating, updating, and deleting data, respectively. The `httpBody` property is used for passing data in a POST or PUT request. Additionally, the `URLRequest` class has additional properties and methods for setting headers, handling redirects, and more.

See Also
- [Apple Developer Documentation - URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Mozilla Developer Network - HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)