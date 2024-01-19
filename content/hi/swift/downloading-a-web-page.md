---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# वेब पेज डाउनलोड करना Swift प्रोग्रामिंग में

## क्या एवं क्यों?

वेब पेज डाउनलोड करना मतलब खास जानकारी को एक सर्वर से अपने डिवाइस पर लाना। यह डाटा विश्लेषण, वेब स्क्रैपिंग, या वेबसाइट के बैकअप के लिए की जाती है। 

## कैसे करें:

डाउनलोड करने हेतु Swift में `URLSession` का प्रयोग हम करेंगे:

```Swift 
import Foundation

let url = URL(string: "http://yourwebsite.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str ?? "")")
    }
}
task.resume()
```
इसे चलाने पर आपको वेबसाइट के HTML कोड मिलेगा।

## गहराई में:

**ऐतिहासिक संदर्भ:** `URLSession` एक हाई-लेवल API है जो वेबसाइट से डाटा हासिल करता है। इसके पहले `NSURLConnection` का इस्तेमाल होता था।  

**विकल्प:** अगर आपको हेडलेस ब्राउजिंग या JavaScript परवाना की जरूरत है, आप `WebKit` या `Puppeteer` का उपयोग कर सकते हैं। 

**कार्यान्वयन विवरण:** `URLSession.shared` सिंगलटन इंस्टांस है जो पूरे आवेदन पर सामान्य वेब कार्यों के लिए उपयोग होता है। अगर आपको अधिक नियंत्रण की आवश्यकता है, तो `URLSession.init(configuration:)` का उपयोग करके अपना सेशन बनाने का विकल्प भी होता है।  

## अधिक जानकारी के लिए:

- [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Swift Web Scraping Guide](https://www.raywenderlich.com/1228252/web-scraping-with-swift)
- [Comparison of different methods for downloading a web page in Swift](https://programmingwithswift.com/how-to-download-a-url-in-swift/)