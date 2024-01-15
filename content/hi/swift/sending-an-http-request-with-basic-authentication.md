---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
html_title:           "Swift: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजने में बेसिक प्रमाणीकरण का उपयोग करने का कारण यह है कि यह आपको उपयोगकर्ता के खाते को सुरक्षित रूप से संपादित करने की अनुमति देता है।

## कैसे करें

बेसिक प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजने के लिए, हम निम्नवत चरणों का पालन कर सकते हैं:

```Swift
// आवश्यक पुस्तकालय आयात करें
import Foundation

// अनुरोध ऑब्जेक्ट बनाएं
let url = URL(string: "https://www.example.com")
var request = URLRequest(url: url!)

// अनुरोध में उपयोगकर्ता नाम और पासवर्ड जोड़ें
let username = "example_user"
let password = "example_password"
// बेसिक प्रमाणीकरण के लिए उपयोगकर्ता नाम और पासवर्ड की संलग्नता
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)
let base64LoginString = loginData!.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

// सत्यापन चालू
URLSession.shared.dataTask(with: request) { data, response, error in
  guard let data = data, let response = response as? HTTPURLResponse, error == nil else {                                                 
    print("error", error ?? "Unknown error")
    return
  }

  // अनुरोध के प्रतिसाद को मानव पठनीय रूप में प्रिंट करें
  if let responseData = String(data: data, encoding: .utf8) {
    print(responseData)
  }
}.resume()
```

आउटपुट:

``` 
Welcome to Example.com!
```

## गहराई में जाएं

बेसिक प्रमाणीकरण एक आम तरीके है उपयोगकर्ता को खाते को सत्यापित करने के लिए HTTP अनुरोध में प्रवेश करने के लिए। यह अनुरोध उपयोगकर्ता के साथ प्रदान किए गए उपयोगकर्ता नाम और पासवर्ड को सर्वर तक भेजता है। यदि यह सही होता है, तो सर्वर स्वीकार करता है और उपयोगकर्ता को अनुमति देता है ताकि वह संपादित कर सके