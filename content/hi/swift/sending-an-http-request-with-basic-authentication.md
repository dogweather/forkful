---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
HTTP अनुरोध के साथ मूल प्रमाणीकरण (Basic Authentication) एक तरीका है जिससे हम किसी सर्वर पर पहुंच प्राप्त कर सकते हैं जो सिर्फ़ प्रमाणित उपयोगकर्ताओं के लिए सुरक्षित होता है। प्रोग्रामर्स इसे सुरक्षित संचार और डेटा सुरक्षा सुनिश्चित करने के लिए करते हैं।

## कैसे: 
यहां स्विफ्ट की एक सामान्य उदाहरण है जिसमें HTTP अनुरोध के साथ मूल प्रमाणीकरण भेजा जा रहा है:

```Swift
import Foundation

let username = "admin"
let password = "admin123"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

let url = URL(string: "https://www.example.com/")!
var request = URLRequest(url: url)
request.httpMethod = "POST"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else { return }
    print(String(data: data, encoding: .utf8) ?? "")
}

task.resume()
```

## गहरा विवेचन: 
1) ऐतिहासिक संदर्भ: मूल प्रमाणीकरण (Basic Authentication) हालांकि सुरक्षा के हिसाब से बेहतर विकल्पों के आविष्कार के बावजूद अभी भी व्यापक रूप से इस्तेमाल किया जाता है।
2) विकल्प: OAuthऔर Bearer Token इसके प्रमुख विकल्प हैं। ये विकल्प अधिक सुरक्षित होते हैं, लेकिन उन्हें लागू करना मूल प्रमाणीकरण से अधिक जटिल हो सकता है।
3) कार्यान्वयन विवरण: उपयोगकर्ता नाम और पासवर्ड को सामान्य पाठ में कोड करके 'Authorization' HTTP हेडर के रूप में जोड़ा जाता है।

## यह भी देखें: 
- [Basic Authentication with Swift - Medium Post](https://medium.com/@javedmultani16/basic-authentication-with-swift-6a3f5fe6a407)
- [HTTP Basic Authentication — MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [URLSession - Swift Documentation](https://developer.apple.com/documentation/foundation/urlsession)