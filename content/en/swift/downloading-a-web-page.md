---
title:                "Swift recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

Downloading web pages is a crucial part of web development and app development. It allows developers to gather information from external sources and display it in their own applications.

## How To

To download a web page in Swift, we can use the `URLSession` class. First, we need to create a `URLSession` object and specify its configuration. Then, we can create a `URL` object with the URL of the webpage we want to download. Finally, we can use the `dataTask(with:)` method on our `URLSession` object to create a data task and download the webpage.

```Swift
// create a URLSession object with default configuration
let session = URLSession(configuration: .default)
// create a URL object with the webpage's URL
let url = URL(string: "https://www.example.com")!
// create a data task to download the webpage
let task = session.dataTask(with: url) { (data, response, error) in
    // handle the downloaded data
    if let error = error {
        // handle any errors
        print("Error: \(error.localizedDescription)")
    } else if let data = data {
        // handle the downloaded data
        print("Data: \(data)")
    }
}
// start the data task
task.resume()
```

The above code will download the webpage and print out the downloaded data. We can also use the `response` object to get information about the server's response, such as its status code and headers.

## Deep Dive

A `URLSession` object manages the network transfer tasks and provides support for the various protocols used by web services. It also allows for customization of the session's behavior, such as specifying a timeout value or allowing for background downloads.

The `URL` class represents a URL and provides methods for creating and managing URLs. The `dataTask(with:)` method creates a data task for the specified URL and returns a `URLSessionDataTask` object.

When the `dataTask(with:)` method is called, the download task will begin immediately and the data will be retrieved in the background. Once the download is complete, the completion handler will be called and we can handle the downloaded data and any errors that may have occurred.

## See Also

For more information and examples on downloading web pages in Swift, check out these resources:

- [Official Apple Documentation for URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Tutorial on Downloading Web Pages in Swift](https://www.raywenderlich.com/1387264-downloading-content-from-the-web-in-swift)
- [Sample Code for Downloading and Displaying Web Pages in Swift](https://github.com/awr/swiftwebview/tree/master/WebViewDemo)