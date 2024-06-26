---
date: 2024-01-20 18:03:26.118583-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Swift \u0915\u0940\
  \ \u092E\u0926\u0926 \u0938\u0947 \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\
  \u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP\
  \ \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E \u092C\u0939\
  \u0941\u0924 \u0938\u0930\u0932 \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\
  \u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:52.917778-06:00'
model: gpt-4-1106-preview
summary: "Swift \u0915\u0940 \u092E\u0926\u0926 \u0938\u0947 \u092C\u0947\u0938\u093F\
  \u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0915\u0947\
  \ \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\
  \u0928\u093E \u092C\u0939\u0941\u0924 \u0938\u0930\u0932 \u0939\u0948\u0964 \u092F\
  \u0939\u093E\u0901 \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948\
  ."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
Swift की मदद से बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना बहुत सरल है। यहाँ एक उदाहरण है:

```Swift
import Foundation

let credentials = "username:password"
let host = "https://api.example.com/data"

if let data = credentials.data(using: .utf8) {
    let credentialsBase64 = data.base64EncodedString()

    var request = URLRequest(url: URL(string: host)!)
    request.httpMethod = "GET"
    request.setValue("Basic \(credentialsBase64)", forHTTPHeaderField: "Authorization")

    let session = URLSession.shared
    let task = session.dataTask(with: request) { (data, response, error) in
        if let error = error {
            print("Error: \(error)")
        } else if let data = data, let dataString = String(data: data, encoding: .utf8) {
            print("Response Data: \(dataString)")
        }
    }
    task.resume()
}
```

जब यह कोड चलता है, यह कुछ इस तरह का आउटपुट देगा:
```
Response Data: यहां आपका प्रतिष्ठान डेटा होगा...
```

## विस्तार से जानकारी:
बेसिक प्रमाणीकरण की शुरुआत 1990 के दशक में हुई थी। यह एक साधारण प्रक्रिया है जिसमें `Authorization` हैडर में Base64 एन्कोडिंग में उपयोगकर्ता नाम और पासवर्ड जोड़े जाते हैं। सुरक्षित HTTP अनुरोधों के लिए आजकल अधिकतर OAuth जैसे जटिल प्रणालियों का उपयोग होता है, पर बेसिक प्रमाणीकरण अभी भी उपयोगी होता है जब आपको सरल प्रमाणीकरण करना होता है। इसे नेटवर्क सुरक्षा के पहलू से देखें तो, यह एन्क्रिप्टेड कनेक्शन (HTTPS) पर चलाना चाहिए, क्योंकि Base64 एन्कोडिंग आसानी से डीकोड हो सकती है।

## यह भी देखें:
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Swift Documentation - URL Loading System](https://developer.apple.com/documentation/foundation/url_loading_system)
- [Base64 Encoding](https://en.wikipedia.org/wiki/Base64)
