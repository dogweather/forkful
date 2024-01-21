---
title:                "Завантаження веб-сторінки"
date:                  2024-01-20T17:45:04.756297-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і Чому?

Downloading a web page means fetching the HTML content of a web page from the Internet. Programmers do it to extract data, interact with web services, or test their sites.

## How to:
## Як це зробити:

Using `URLSession`, we can get content easily.

```Swift
import Foundation

let url = URL(string: "https://example.com")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Error downloading webpage: \(error)")
        return
    }
    if let data = data, let webpageContent = String(data: data, encoding: .utf8) {
        print(webpageContent)
    }
}

task.resume()
```

Expect something like this as output (truncated for brevity):

```Swift
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive:
## Детально:

Back in the day, web pages were simpler, and we used libraries like `NSURLConnection`. Now, `URLSession` is the go-to in Swift, more powerful and flexible.

There are other ways to download content, like using `WebKit` for JavaScript-heavy pages, or third-party libraries such as Alamofire for complex networking tasks.

Implementation-wise, always remember to run network calls on background threads to keep your UI snappy. Handle `Data`, `URLResponse`, and `Error` correctly to manage edge cases. Consider also HTTP status codes and MIME types to process data right.

## See Also:
## Додаткові матеріали:

- Official `URLSession` documentation: https://developer.apple.com/documentation/foundation/urlsession
- Swift API design guidelines: https://swift.org/documentation/api-design-guidelines/
- Alamofire GitHub repository: https://github.com/Alamofire/Alamofire