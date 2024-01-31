---
title:                "Downloading a web page"
date:                  2024-01-20T17:44:44.664371-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a web page means grabbing the data from the web and bringing it into your app. Programmers do it to fetch content, interact with online services, or scrape data.

## How to:
Let's use `URLSession` to do the job. Swift makes it straight to the point.

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Error:", error)
        return
    }

    if let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) {
        if let mimeType = httpResponse.mimeType, mimeType == "text/html",
           let data = data, let string = String(data: data, encoding: .utf8) {
            print("Downloaded web page content:")
            print(string)
        } else {
            print("Invalid MIME type or encoding.")
        }
    } else {
        print("Server responded with error.")
    }
}
task.resume()
// Make sure the playground keeps running until the task completes
RunLoop.current.run()
```

Sample output might look like this:

```
Downloaded web page content:
<!doctype html>...
```

## Deep Dive
The `URLSession` API has been around since iOS 7 and macOS 10.9. It was a game-changer back then, replacing the older, more cumbersome `NSURLConnection`. While `URLSession` is powerful and flexible, you could also consider third-party libraries like Alamofire for more complex networking needs. 

When implementing, remember that network requests are asynchronous. This means your app can carry on with other tasks while the server gets back to you. Also, using `URLSession` properly involves handling errors gracefully and checking the server's response status. The MIME type checking is crucial to ensure you're receiving HTML, not other file types like JSON or an image.

## See Also
Dive deeper or explore alternatives:
- Apple's `URLSession` documentation: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Swift networking with Alamofire: [Alamofire](https://github.com/Alamofire/Alamofire)
- Swift async/await pattern for `URLSession` in iOS 15+: [URLSession async/await](https://developer.apple.com/videos/play/wwdc2021/10054/)
