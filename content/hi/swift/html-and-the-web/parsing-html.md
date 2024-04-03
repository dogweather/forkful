---
date: 2024-01-20 15:34:42.377782-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Swift \u092E\
  \u0947\u0902 HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0939\u092E SwiftSoup \u091C\u0948\u0938\u0947 third-party\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092A\u0939\u0932\u0947 CocoaPods \u092F\u093E Swift Package\u2026"
lastmod: '2024-03-13T22:44:52.914524-06:00'
model: unknown
summary: "Swift \u092E\u0947\u0902 HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0939\u092E SwiftSoup \u091C\u0948\
  \u0938\u0947 third-party \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \n\n\u092A\u0939\u0932\u0947 CocoaPods \u092F\u093E Swift\
  \ Package Manager \u0938\u0947 SwiftSoup \u0907\u0902\u0938\u094D\u091F\u0949\u0932\
  \ \u0915\u0930\u0947\u0902\u0964\n\n\u092B\u093F\u0930, SwiftSoup \u0915\u093E \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0941\
  \u090F HTML \u0938\u0947 \u0921\u0947\u091F\u093E \u0928\u093F\u0915\u093E\u0932\
  \u0928\u093E \u0907\u0938 \u092A\u094D\u0930\u0915\u093E\u0930 \u0939\u094B\u0917\
  \u093E."
title: "HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
weight: 43
---

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
