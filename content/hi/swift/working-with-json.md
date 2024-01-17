---
title:                "Json के साथ काम करना"
html_title:           "Swift: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/working-with-json.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
JSON काम करना क्या है और प्रोग्रामर्स उसे क्यों करते हैं।

(JSON, JavaScript Object Notation का संक्षिप्त रूप है। यह एक स्ट्रिंग के रूप में डेटा जोड़े वाला एक लाइटवेट डेटा एक्सचेंज फॉर्मेट है। प्रोग्रामर्स इसका उपयोग डेटा को अपने ऐप्लिकेशन या वेबसाइट के अन्दर संग्रहीत करने के लिए करते हैं।)

## कैसे करें:
यहां हम आपको JSON डेटा को संग्रहीत करने और उसे स्वतंत्र अनुभव करने का एक सरल तरीका दिखाएंगे।

```Swift
let json = "{\"name\":\"John\", \"age\":25}"
if let data = json.data(using: .utf8) {
    let user = try? JSONDecoder().decode(User.self, from: data)
    print(user?.name) // Output: John
    print(user?.age) // Output: 25
}
```

इस कोड में, हमने स्ट्रिंग के एक डिक्शनरी में जोन डेटा संग्रहीत किया है। फिर हमने वह डेटा decode किया और उसे उसे User object में परिवर्तित किया। आप इसी तरह से अपनी JSON डेटा को स्वतंत्रता से पढ़ सकते हैं और अपने ऐप में उसका उपयोग कर सकते हैं।

## गहराई से जानें:
जब आपको बहुत सारी डेटा संग्रहीत करनी हो और आपके पास कुछ प्रतिस्थितिगत डेटा हो तो JSON एक बहुत ही अच्छा चयन हो सकता है। आप अपनी JSON डेटा को विभिन्न तरीकों से संग्रहीत कर सकते हैं जैसे कि XML या CSV।

## देखें भी:
यदि आपको अधिक जानकारी चाहिए तो आप [Swift's documentation on JSON](https://developer.apple.com/documentation/foundation/archives_and_serialization/json) देख सकते हैं। इसके अलावा, आप [SwiftyJSON](https://github.com/SwiftyJSON/SwiftyJSON) की मदद से रीखा जा सकता है जो कि JSON संग्रह के लिए बहुत ही उपयोगी है।