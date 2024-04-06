---
date: 2024-01-20 18:00:46.802210-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) HTTP \u0905\
  \u0928\u0941\u0930\u094B\u0927 (requests) 1990 \u0915\u0947 \u0926\u0936\u0915 \u0938\
  \u0947 \u0935\u0947\u092C \u0915\u093E \u0939\u093F\u0938\u094D\u0938\u093E \u0939\
  \u0948\u0902\u0964 \u0906\u091C \u0939\u092E NSURLSession (Swift \u092E\u0947\u0902\
  \ URLSession) \u091C\u0948\u0938\u0947 \u092E\u0949\u0921\u0930\u094D\u0928 API\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\
  \u0947\u2026"
lastmod: '2024-04-05T22:51:07.578579-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) HTTP \u0905\u0928\u0941\
  \u0930\u094B\u0927 (requests) 1990 \u0915\u0947 \u0926\u0936\u0915 \u0938\u0947\
  \ \u0935\u0947\u092C \u0915\u093E \u0939\u093F\u0938\u094D\u0938\u093E \u0939\u0948\
  \u0902\u0964 \u0906\u091C \u0939\u092E NSURLSession (Swift \u092E\u0947\u0902 URLSession)\
  \ \u091C\u0948\u0938\u0947 \u092E\u0949\u0921\u0930\u094D\u0928 API \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902, \u091C\u094B \u092A\u0939\u0932\u0947 \u0915\u0947 NSURLConnection\
  \ \u0938\u0947 \u090F\u0921\u0935\u093E\u0902\u0938 \u0939\u0948\u0964 Alamofire\
  \ \u091C\u0948\u0938\u0947 third-party libraries \u092D\u0940 \u092A\u0949\u092A\
  \u0941\u0932\u0930 \u0939\u0948\u0902\u0964 URLSession \u092E\u0947\u0902, \u0906\
  \u092A \u0921\u0947\u091F\u093E \u091F\u093E\u0938\u094D\u0915\u094D\u0938, \u0905\
  \u092A\u0932\u094B\u0921 \u091F\u093E\u0938\u094D\u0915\u094D\u0938, \u0921\u093E\
  \u0909\u0928\u0932\u094B\u0921 \u091F\u093E\u0938\u094D\u0915\u094D\u0938, \u0914\
  \u0930 \u0938\u094D\u091F\u094D\u0930\u0940\u092E\u093F\u0902\u0917 \u091F\u093E\
  \u0938\u094D\u0915\u094D\u0938 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902\u0964 \u0924\u094D\u0930\u0941\u091F\u093F \u0939\u0948\u0902\u0921\u0932\
  \u093F\u0902\u0917 \u0914\u0930 HTTP status codes \u0915\u093E \u092A\u094D\u0930\
  \u094B\u092A\u0930 \u0930\u0947\u0938\u094D\u092A\u0949\u0928\u094D\u0938 \u0915\
  \u094B\u0921 \u0915\u0947 \u0938\u093E\u0925 \u092A\u094D\u0930\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E \u092E\u0939\u0924\u094D\u0935\u092A\u0942\u0930\u094D\
  \u0923 \u0939\u0948\u0964 \u0906\u092A json, xml \u092F\u093E \u0905\u0928\u094D\
  \u092F \u092A\u094D\u0930\u0915\u093E\u0930 \u0915\u0947 \u0921\u0947\u091F\u093E\
  \ \u0915\u094B serialize \u0914\u0930 deserialize \u092D\u0940 \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\u093F \u0938\u0930\u094D\
  \u0935\u0930 \u0938\u0947 \u0909\u0938\u0947 \u092A\u094D\u0930\u094B\u0938\u0947\
  \u0938 \u0915\u0930 \u0938\u0915\u0947\u0902\u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
```Swift
import Foundation

// URL बनाएँ
if let url = URL(string: "https://api.example.com/data") {
    
    // URLRequest बनाएँ
    var request = URLRequest (url: url)
    request.httpMethod = "GET" // या "POST", "PUT", "DELETE", etc.
    
    // URLSession के साथ HTTP request भेजें
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        
        // यदि कोई त्रुटि है, तो उसे प्रिंट करें। 
        if let error = error {
            print("Error: \(error)")
            return
        }
        
        // सर्वर का जवाब चेक करें और डेटा को प्रिंट करें
        if let response = response as? HTTPURLResponse, response.statusCode == 200 {
            if let data = data, let body = String(data: data, encoding: .utf8) {
                print("Response Body: \n\(body)")
            }
        } else {
            print("HTTP Request Failed")
        }
    }
    
    // टास्क शुरू करें
    task.resume()
}
```
`Sample Output`:
```
Response Body: 
{
  "key": "value"
}
```

## Deep Dive (गहराई से जानकारी):
HTTP अनुरोध (requests) 1990 के दशक से वेब का हिस्सा हैं। आज हम NSURLSession (Swift में URLSession) जैसे मॉडर्न API का इस्तेमाल करते हैं, जो पहले के NSURLConnection से एडवांस है। Alamofire जैसे third-party libraries भी पॉपुलर हैं।

URLSession में, आप डेटा टास्क्स, अपलोड टास्क्स, डाउनलोड टास्क्स, और स्ट्रीमिंग टास्क्स कर सकते हैं। त्रुटि हैंडलिंग और HTTP status codes का प्रोपर रेस्पॉन्स कोड के साथ प्रयोग करना महत्वपूर्ण है। आप json, xml या अन्य प्रकार के डेटा को serialize और deserialize भी कर सकते हैं ताकि सर्वर से उसे प्रोसेस कर सकें।

## See Also (और देखें):
- [Apple's URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Ray Wenderlich's Networking with URLSession](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)
