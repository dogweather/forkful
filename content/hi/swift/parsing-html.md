---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:34:42.377782-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML पार्सिंग वेबसाइट के डाटा को संस्करित करने की प्रक्रिया है। प्रोग्रामर्स ऐसा तब करते हैं जब उन्हें वेब पेज से सामग्री निकालनी होती है।

## How to: (कैसे करें:)
Swift में HTML पार्स करने के लिए हम SwiftSoup जैसे third-party लाइब्रेरी का उपयोग कर सकते हैं। 

पहले CocoaPods या Swift Package Manager से SwiftSoup इंस्टॉल करें।

फिर, SwiftSoup का इस्तेमाल करते हुए HTML से डेटा निकालना इस प्रकार होगा:

```Swift
import SwiftSoup

let htmlString = """
    <html>
        <head>
            <title>नमस्ते Swift</title>
        </head>
        <body>
            <p class='greeting'>Hello, World!</p>
        </body>
    </html>
    """

do {
    let doc: Document = try SwiftSoup.parse(htmlString)
    if let title = try doc.title() {
        print(title)  // Output: नमस्ते Swift
    }
    
    if let greeting = try doc.getElementsByClass("greeting").first()?.text() {
        print(greeting)  // Output: Hello, World!
    }
} catch Exception.Error(let type, let message) {
    print("Type: \(type)")
    print("Message: \(message)")
} catch {
    print("error")
}
```

## Deep Dive (विस्तार से जानकारी):
HTML पार्सिंग की आवश्यकता तब आती है जब हमें वेबडेटा को अपने एप्लीकेशन के फॉर्मेट में बदलना होता है। पुराने समय में, रेगेक्स (Regular Expressions) का इस्तेमाल होता था, पर वह अविश्वसनीय और जटिल था। SwiftSoup जैसी लाइब्रेरीज ने इस काम को आसान और अधिक सटीक बना दिया है। DOM (Document Object Model) पर आधारित ये लाइब्रेरीज parse, search, और manipulate HTML को अधिक सहजता से करने में हमारी मदद करती हैं।

## See Also (अन्य संसाधन):
- SwiftSoup गिटहब पेज: [SwiftSoup on GitHub](https://github.com/scinfu/SwiftSoup)
- HTML पार्सिंग के लिए XMLParser डॉक्युमेंटेशन: [Apple's XMLParser](https://developer.apple.com/documentation/foundation/xmlparser)
- Swift के SPM (Swift Package Manager) के बारे में जानकारी: [Swift Package Manager](https://swift.org/package-manager/)
