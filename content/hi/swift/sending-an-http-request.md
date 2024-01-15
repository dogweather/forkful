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

## क्यों

एक व्यक्ति हमेशा ही वेब क्लाइंट से डेटा प्राप्त करना या डेटा को सर्वर पर भेजना चाहता है, तो उन्हें HTTP रिक्वेस्ट भेजने की आवश्यकता होती है। HTTP रिक्वेस्ट भेजने से आप अपने ऐप्स या वेबसाइट में डायनामिक सामग्री को डाउनलोड कर सकते हैं या यूजर के द्वारा दर्ज किया गया डेटा को सर्वर पर स्टोर कर सकते हैं।

## कैसे करें

```Swift
let url = URL(string: "https://www.example.com/data")!
var request = URLRequest(url: url)
request.httpMethod = "GET"

let session = URLSession.shared
let task = session.dataTask(with: request) { data, response, error in
    guard let data = data, error == nil else {                                                  
        // handle error here
    }
    
    // use response and data here
    print(response)
    let output = String(data: data, encoding: .utf8)
    print(output)
}

task.resume()
```

उपरोक्त कोड स्निपेट में, हमने किसी भी श्रोत से डेटा प्राप्त करने के लिए सर्वर पर GET रिक्वेस्ट भेजी है। हमने एक URL बनाया है जिससे हम डेटा डाउनलोड कर सकते हैं और फिर हमने उस URL को कैसे खोलना है की निर्देशिका दिखायी है। आगे भेजे गए संदेश का जवाब, डेटा और वेबसाइट के URL के रूप में हमसे प्रकाशित भी होता है। आप वेबसाइट के डेटा को डाउनलोड करने के लिए प्रति session का उपयोग कर सकते हैं और इस वेब ब्राउज़र के अंदर ऐसा कुछ कैसे करते हैं जो एक सेशन खोल सकता है। विश्लेषण करते समय खोलने की योग्यता बनाएँ जिसका हम इस्तेमाल करते हैं।

## गहराई में जाएं

HTTP रिक्वेस्ट भेजते समय, आपको उन्हें `GET`, `POST`, `PUT` या `DELETE` क्यों भेजने के बारे में जानना होगा। `GET`