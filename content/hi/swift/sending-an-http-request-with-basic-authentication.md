---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:03:26.118583-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध के साथ बेसिक प्रमाणीकरण का मतलब है वेब सर्वर से डेटा माँगते समय उपयोगकर्ता नाम और पासवर्ड प्रदान करना। प्रोग्रामर इसका इस्तेमाल डेटा सुरक्षित रखने और अनधिकृत पहुँच से बचने के लिए करते हैं।

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
