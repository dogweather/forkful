---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Swift में HTTP अनुरोध भेजना: एक पूर्ण गाइड

## क्या और क्यों?

HTTP अनुरोध, वेब सर्वर को डेटा अनुरोध करने के लिए इस्तेमाल की जाती है। प्रोग्रामर्स इन्हें इस्तेमाल करते हैं क्योंकि वे API के माध्यम से सर्वर से डेटा मांग सकते हैं और वापसी में JSON, XML, HTML इत्यादि प्राप्त कर सकते हैं। 

## कैसे:

Swift में, हम एक URL और URLRequest उत्पन्न करते हैं, URLSession से एक URLSessionDataTask बनाते हैं, और अंत में task.resume() कॉल करते हैं। चलिए इसे कोड में देख लेते हैं: 

```Swift 
let url = URL(string: "https://your-api-url.com")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        print("Data: \(data)")
    }
}
task.resume()
```

इससे आपको डेटा या एरर मिलेगा, जिसे आप अगले कोड में प्रिंट कर सकते हैं। 

## गहराई में:

HTTP अनुरोध और इसे Swift में कैसे उपयोग किया जाता है, इसकी खोज दकियानूसी WWW में 1990 में हुई थी। 

HTTP के विकल्पों में gRPC, WebSockets और GraphQL (!) शामिल हैं। 

किसी API को कॉल करने में URLSession का उपयोग करने के बजाय, आप Alamofire जैसे बाहरी पुस्तकालयों का उपयोग भी कर सकते हैं। Alamofire आपको अधिक नियंत्रण और मजबूत फ़ंक्शन प्रदान करता है, लेकिन मूल URLSession API का ज्ञान होना अच्छी बात है।

## भी देखें:

1. Apple Developer Documentation: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
2. Swift HTTP Networking: A Comprehensive Guide [Article](https://www.raywenderlich.com/1548775-urlsession-tutorial-getting-started)
3. HTTP and REST [Video Lecture](https://www.youtube.com/watch?v=1XY1L7lP_8I)