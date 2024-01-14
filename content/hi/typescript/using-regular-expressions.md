---
title:                "TypeScript: नियमित अभिव्यक्तियों का उपयोग"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

हम लोग आमतौर पर जब भी कोई लॉजिकल या पैटर्न मिलते हैं तो उनको संशोधित या खोजने के लिए रेगुलर एक्सप्रेशन्स का उपयोग करते हैं। ये एक अधिक प्रभावी तकनीक है जो हमें एक से अधिक स्ट्रिंग मैच करने में मदद करती है।

## कैसे करें

```TypeScript
const regex = /hindi/g; 
// एक साधारण रेगुलर एक्सप्रेशन बनाना

const str = "हम भारतीय हैं"; 
// एक स्ट्रिंग बनाना

const result = str.match(regex); 
// रेगुलर एक्सप्रेशन को स्ट्रिंग में मैच करना

console.log(result); 
// ["हिंदी"]
```

## गहराई मंडली

रेगुलर एक्सप्रेशन्स के साथ काम करना थोड़ा चुनौतीपूर्ण हो सकता है। इसलिए यह महत्वपूर्ण है कि हम समझें कि कैसे रेगुलर एक्सप्रेशन्स काम करता है और कैसे हम इसे अपने प्रोग्राम में उपयोग कर सकते हैं। हमने प्रकार, मेटाचॉड्स, लुप, और अन्य बहुत सारे आंकड़े आपको दिए हैं जो आपको रेगुलर एक्सप्रेशन्स की गहराई से अधिक अनुकरण कौशल को बढ़ा सकते हैं।

## इसके अलावा देखें

[Typescript ट्यूटोरियल](https://www.tutorialspoint.com/typescript/) 

[रेगुलर एक्सप्रेशन्स गाइड](https://javascript.info/regular-expressions) 

[Typescript सॉनलाइन संचालन](https://www.typescriptlang.org/play)