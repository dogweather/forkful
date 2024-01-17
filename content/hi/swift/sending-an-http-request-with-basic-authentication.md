---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना।"
html_title:           "Swift: बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना।"
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना।"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजने के लिए बेसिक प्रमाणीकरण के साथ सिंडिंग क्या है, यह समझने के बाद प्रोग्रामर सिंडिंग क्यों करते हैं।

## कैसे करें:

```Swift
let url = URL(string: "http://www.example.com")
let username = "username"
let password = "password"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

var request = URLRequest(url: url!)
request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {
        print("Error: \(error ?? "Unknown error" as! Error)")
        return
    }
    
    if let httpStatus = response as? HTTPURLResponse, httpStatus.statusCode != 200 {
        print("Error: HTTP status code is \(httpStatus.statusCode)")
        print("Response: \(response!)")
    }
    
    let responseString = String(data: data, encoding: .utf8)
    print("Response: \(responseString!)")
}
task.resume()
```

यह कोड उदाहरण बताता है कि हम कैसे बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेज सकते हैं। इसमें हम URL और क्रेडेंशियल्स का इस्तेमाल करते हैं जो उपयोगकर्ता का नाम और पासवर्ड को कॉलन द्वारा जोड़ता है। फिर हम अपने अनुरोध को आईडीओ बिल्क अपलोड करते हुए अविनाश को  भेजते हैं और उसके बाद हम अप्रत्युक्त उत्पाद कॉल दर्शा सकते हैं। 

```Swift
Response: Optional(The HIV Simple/Basic Web server, example page)
```

यह नतीजा उस वेबसाइट पर पूरी तरह से अभिव्यक्त करता है जो हमने भेजा है। हम आसानी से बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेज सकते हैं और उसका प्रतिक्रिया प्राप्त कर सकते हैं। 

## गहराई में जाएँ:

HTTP बेसिक प्रमाणीकरण के साथ अनुरोध भेजने के अतिरिक्त, कुछ अविकल्प भी हैं जो इस काम को करने के लिए उपयोग किए जा सकते हैं। एक विकल्प है डाइजेस्ट प्रमाणीकरण जो भी उपयोगकर्ता का नाम और पासवर्ड दर्शाता है, लेकिन यह अधिक सुरक्षित है। इसके अलावा, आप C या Java में हाईरयरसीएस भी उपयोग कर सकते हैं जो बहुत सुरक्षित होता है। 

HTTP बेसिक प्रमाणीकरण को ध्यान में रखते हुए जिस URL से आप अनुरोध भेजते हैं, आपका अनुरोध और उससे जुड़ी सभी जानकारी अन्य लोगों को मिल सकती है। इसलिए, आपको अपने अनुरोध को बेहतर सुरक्षित करने के लिए सुनिश्चित होना चाहिए। 

## इससे जुड़े प्रमाण:

अधिक जानकारी के लिए निम्नलिखित स्रोतों पर जाएँ:

- एप्पलआईओएस डेवलपर दस्तावेज़ीकरण कंटेंट गाइड।
https://developer