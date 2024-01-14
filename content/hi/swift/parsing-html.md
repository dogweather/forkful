---
title:                "Swift: हटाना HTML"
simple_title:         "हटाना HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों
क्या आप कभी सोचा है कि आप किसी वेबसाइट पर देखा गया सामग्री को कैसे पूर्णरुप से उपयोग कर सकते हैं? संसाधनों को बाछवाने या वेबसाइट से डेटा को निकालने के लिए आपको HTML पार्सिंग की आवश्यकता होगी। यह आपको विभिन्न वेबसाइटों पर उपलब्ध सामग्री के साथ सामंजस्य बनाने और उसका उपयोग करने की अनुमति देता है।

## कैसे करें
जब आप Swift में HTML पार्सिंग कर रहे हों तो सबसे पहले आपको हमेशा HTML स्रोत को NSString या String ऑब्जेक्ट के रूप में प्राप्त करने की आवश्यकता होगी। इसके बाद हमें कुछ पाठ या उसे वेबसाइट की पेज पर प्रतिस्थापित करने के लिए ध्यान से HTML पार्स करने की आवश्यकता होती है। निम्नलिखित कोड ब्लॉक में दिखाए गए उदाहरण में हम एक दुसरे पेज की कड़ियों को ध्यान से पार्स कर रहे हैं:

```Swift
let htmlString = "<html><body><h1>Welcome to my website</h1><p>This is a paragraph</p></body></html>"
let data = htmlString.data(using: .utf8)
let options: [NSAttributedString.DocumentReadingOptionKey: Any] = [.documentType: NSAttributedString.DocumentType.html]
let attributedString = try! NSAttributedString(data: data!, options: options, documentAttributes: nil)
print(attributedString.string)
```

ऊपर दिए गए कोड में, हमने HTML पार्सिंग करने के लिए NSAttributedString के साथ data और options का उपयोग किया है। परिणाम print करने पर हमारे कोड ने निम्नलिखित परिणाम प्रस्तुत किया है:
Welcome to my website
This is a paragraph

## गहराई में जाओ
एक आधिकारिक तरीके से HTML पार्सिंग करने के लिए, आपको समझने होगा कि HTML क्‍या है और वह कैसे काम करता है। HTML संरचना को जानने के बाद आप उसे पार