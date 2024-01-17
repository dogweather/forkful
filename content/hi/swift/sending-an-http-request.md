---
title:                "एक http अनुरोध भेजना"
html_title:           "Swift: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

क्या और क्यों?
HTTP अनुरोध भेजना क्या है और क्यों प्रोग्रामर्स इसे करते हैं। एचटीटीपी रिक्वेस्ट भेजने का अर्थ होता है कि हम आपत्तिजनक सर्वर से संवाद कर सकते हैं और इंटरनेट से डेटा को हासिल कर सकते हैं। यह सबसे आम तरीका है जिससे हम अपने ऐप्स के साथ अन्य सर्वर संसाधनों से डेटा को एपीआई के माध्यम से जोड़ सकते हैं।

कैसे करें:
```Swift
let url = URL(string: "https://www.example.com/api/users")
let request = URLRequest(url: url!)
let session = URLSession.shared
let task = session.dataTask(with: request, completionHandler: {
    (data, response, error) in
        if error != nil {
            print("Error: \(error)")
        } else {
            let httpResponse = response as? HTTPURLResponse
            print("Status code: \(httpResponse?.statusCode)")
            print("Response: \(data)")
        }
})
task.resume()
```

विवरण में समायोजन:
HTTP अनुरोध भेजने का इतिहास साल १९९५ में प्रकट हुआ जब एलाईएमएस प्रोसेसर्स ने अपने वेब सर्वर्स के साथ आम भाषा अनुप्रयोग के रूप में संवाद स्थापित किया। अब एचटीटीपी अनुरोध भेजना सबसे सामान्य तरीका है जिससे हम अपने ऐप्स के साथ अन्य सर्वर संसाधनों से कम्प्यूटिंग डेटा का उपयोग कर सकते हैं। इसके अलावा, कई लोग डेटा भागता के रूप में भी एचटीटीपी अनुरोध भेजते हैं।

और देखें:
अधिक जानकारी के लिए, [Apple Documentation on URLSession](https://developer.apple.com/documentation/foundation/urlsession) और [HTTP Request Methods](https://www.tutorialspoint.com/http/http_methods.htm) को देखें।