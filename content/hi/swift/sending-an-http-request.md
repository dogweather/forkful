---
title:                "Swift: एक Http अनुरोध भेजना"
simple_title:         "एक Http अनुरोध भेजना"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

अभी आप वेब डेवलपमेंट की दुनिया में बहुत अच्छे 平रे से चल रहे होंगे जहाँ HTTP की रीक्वेस्ट भरता हो गया होंगा। ये बहुत ही महत्वपूर्ण हैं जब आप सर्वर से डाटा लेने या भेजने की आवश्यकता होती है। यह एक सधी सी प्रकिया है लेकिन उसे समझना बहुत ही ज़रूरी है।

## कैसे

आज हम स्विफ्ट में कैसे HTTP रीक्वेस्ट भेज सकते हैं उसके बारे में बात करेंगे। आपको स्विफ्ट कोड के भीतर HTTP रीक्वेस्ट भेजने के लिए NSURLSession और NSURLRequest की ज़रूरत होती है। नीचे दिए गए कोड स्निपेट में हम एक GET रीक्वेस्ट की उदाहरण देखेंगे।

```Swift
// NSURLSession का नया इंस्टेंस बनायें
let session = URLSession.shared
// एक NSURLRequest बनाएं
let request = URLRequest(url: URL(string: "http://www.example.com")!)
// NSURLSessionDataTask का नया इंस्टेंस बनाएं
let task = session.dataTask(with: request) { data, response, error in
    // रेस्पॉन्स को चेक करें
    if let httpResponse = response as? HTTPURLResponse {
        // सफलतापूर्वक सर्वर से कनेक्ट करने की स्थिति देखें
        if httpResponse.statusCode == 200 {
            // डेटा को प्रिंट करें
            if let data = data {
                print(data)
            }
        }
    }
}
// रीक्वेस्ट भेजें
task.resume()
```

ऊपर दिए गए कोड में हम स्विफ्ट के URLSession और URLRequest का इस्तेमाल करके एक GET रीक्वेस्ट को भेजते हैं। हम रिस्पॉन्स को चेक करते हैं और फिर सर्वर से कनेक्ट करने की स्थिति को देखते हैं। फिर डेटा को प्रिंट करते हैं। आप फिर उस डेटा को अपनी आवश्यकतानुसार उपयोग कर सकते हैं।

## गहरी जानकारी

HTTP रीक्वेस्ट भेजने के बारे