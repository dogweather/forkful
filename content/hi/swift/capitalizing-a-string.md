---
title:                "एक स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Swift: एक स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "एक स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
`String` उच्चाक्षर करना एक प्रोसिजर है जो किसी शबद के पहले अक्षर को बड़ा करता है। प्रोग्रामर्स इसे उपयोगकर्ता इनपुट को फॉर्मैट करने और विशिष्ट तरीके से डिस्प्ले करने के लिए इस्तेमाल करते हैं। 

## मार्गदर्शन:
Swift में, आप `capitalized` गुण उपयोग करके string को कैपिटलाइज़ कर सकते हैं। यहाँ एक उदाहरण है:

```Swift
let greeting = "नमस्ते, दुनिया!"
let capitalizedGreeting = greeting.capitalized
print(capitalizedGreeting)
// Outputs "नमस्ते, दुनिया!"
```
जैसा कि आप देख सकते हैं, पहला अक्षर प्रत्येक शब्द का उच्चाक्षर हो गया है। 

## गहराई में:
यद्यपि Swift अच्छा स्थान है शुरुआत करने के लिए, ध्यान दें कि केवल एक या दो स्विफ्ट संयोगों के द्वारा यह कार्य नहीं किया जा सकता है। पहले, Swift का 'String' क्लास बहु-बाइट UTF8 चरित्रों को सही ढंग से संभाल सकता है, जो कि ऑटोमेटिक उच्चाक्षरण के लिए महत्वपूर्ण है।दूसरा, `capitalized` गुण "उच्चाक्षर अक्षर अनुसार" काम करता है, जो कि मुख्य रूप से पश्चिमी भाषा लिपियों के साथ काम करता है, लेकिन दूसरी भाषाओं में भ्रमित हो सकता है। 

## और भी देखें:
यदि आपने किसी विशिष्ट भाषा में उच्चाक्षरण करने की जरूरत है, तो [Apple's Internationalization and Localization Guide](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPInternational/Introduction/Introduction.html) एक महत्वपूर्ण संसाधन है। इसके अलावा, [Swift String documentation](https://developer.apple.com/documentation/swift/string) भी विस्तार में वर्णमाला और कैसे नाम दिए जाते हैं, पर गहराई में जाता है।