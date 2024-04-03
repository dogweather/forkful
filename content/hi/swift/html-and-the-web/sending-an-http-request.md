---
date: 2024-01-20 18:00:46.802210-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.912995-06:00'
model: gpt-4-1106-preview
summary: .
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
