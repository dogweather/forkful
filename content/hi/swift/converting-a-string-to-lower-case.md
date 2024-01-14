---
title:                "Swift: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

एक्स्टंशन्ड्रोज़ स्ट्रिंग को प्रभावी ढंग से लोअर केस में परिवर्तित करने में आपको क्या लाभ हो सकता है।

## कैसे करें

मेनिपुलेशन के द्वारा स्ट्रिंग को लोअर केस में परिवर्तित करने के लिए, आपको निम्नलिखित तरीकों का अनुसरण करना होगा:

```Swift
let string = "Hello World"
let lowercasedString = string.lowercased()

print(lowercasedString) // output: hello world
```

इस उदाहरण में, हमने "lowercased" फंक्शन का उपयोग किया है जो स्ट्रिंग को लोअर केस में परिवर्तित करता है। इसके अतिरिक्त, हम "String" डेटा टाइप के एक्स्टेंशन "lowercased" भी प्रयोग कर सकते हैं।

आप अपनी कस्टम फंक्शन भी बना सकते हैं जो स्ट्रिंग को लोअर केस में परिवर्तित करता है, जैसे:

```Swift
extension String {
    func convertToLowercase() -> String {
        return self.lowercased()
    }
}

let string = "Hello World"
let lowercasedString = string.convertToLowercase()

print(lowercasedString) // output: hello world
```

## गहराई में जाएं

"lowercased" फंक्शन से हम स्ट्रिंग को लोअर केस में परिवर्तित कर सकते हैं लेकिन इसमें क्या होता है।

स्ट्रिंग को लोअर केस में परिवर्तित करने के लिए, हम स्ट्रिंग में से सभी अक्षरों को लोअर केस में बदलते हैं और विशेष वाक्यांश जो मूल स्ट्रिंग में हैं अभी भी वहीं रहते हैं। यह आपके स्ट्रिंग प्रोसेसिंग को सरल बनाता है और आप इसका अनुप्रयोग अपनी आवश्यकताओं के अनुसार कर सकते हैं।

## देखें भी

आपको अधिक स्ट्रिंग के सम्बन्धित विषयों में जानने के लिए निम्नलिखित लिंकों पर जाना