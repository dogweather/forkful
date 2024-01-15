---
title:                "Downloading a web page"
html_title:           "Swift recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why

If you're a web developer or even just a curious internet user, you may want to download a web page for various reasons. Perhaps you want to analyze the HTML structure, check for broken links, or simply save a local copy for offline viewing.

## How To

Downloading a web page in Swift is a relatively simple process that can be done using the URLSession framework. First, we need to create a URL for the web page we want to download. Then, we initialize a URLSession object and use it to create a data task, passing in the URL we just created. Finally, we use the resume() method to start the data task. Here's an example:

```Swift
let url = URL(string: "https://www.example.com")
let session = URLSession(configuration: .default)
let task = session.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        // Do something with the data here
    }
}
task.resume()
```

In the above example, we create a data task that will download the web page specified by the URL. Once the download is complete, the closure is executed and we can access the downloaded data.

To save the downloaded data to a file, we can use the FileManager class. Here's an example of how we can save the downloaded web page as an HTML file:

```Swift
let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!
let filePath = documentsDirectory.appendingPathComponent("example.html")
try data?.write(to: filePath)
```

This will create a file called "example.html" in the app's document directory with the downloaded data. From here, we can do whatever we want with the file.

## Deep Dive

Behind the scenes, the URL session created in the above example uses the HTTP protocol to make a request to the server and download the web page. The data that is returned is in the form of a Data object, which can then be converted to a String or other data type if needed.

There are also various options available for configuring the URL session, such as setting a timeout for the request or handling redirections. For more advanced usage, you can also implement delegate methods to be notified of the progress of the download.

## See Also

- [Official URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Medium Article on Downloading Web Pages in Swift](https://medium.com/@sdrzn/swift-downloading-the-contents-of-webpage-411c76f1bdf8)
- [Stack Overflow Post on Saving a File from URL in Swift](https://stackoverflow.com/questions/24231680/loading-downloading-image-from-url-on-swift)