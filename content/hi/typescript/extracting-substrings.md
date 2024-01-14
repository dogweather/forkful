---
title:                "TypeScript: उपस्थपना उत्पादित करना"
simple_title:         "उपस्थपना उत्पादित करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें हस्ताक्षर में से एक छोटा सा टुकड़ा या शब्द निकालने की ज़रूरत पड़ सकती है। ये सीखना उसके लिए जरूरी है क्योंकि ये हमारे कोड को अधिक स्लिम और अच्छा बनाता है।

## कैसे करें

स्ट्रिंग का `substring` अन्य स्ट्रिंग से एक से ज़्यादा चर निकालने के लिए उपयोग किया जाता है। निम्नलिखित कोड ब्लॉक में दिखाए गए हैं: 

```TypeScript
// स्ट्रिंग बनाएं
let string = "हमारे स्ट्रिंग से डेटा निकालें।"

// स्ट्रिंग से निकाले गए डेटा का उपयोग करें
console.log(string.substring(3,11));
```
आउटपुट: देते समय

आगर आप कॉंपाइल करते हैं तो आपको निम्नलिखित मिलेंगे:

```
्कर्तव्य
ेते समय
```

## गहराई में जाएं

अधिक सीखने के लिए, हम `substring` कैसे काम करता है उसके आंतरिक गणितीय गहराई में जाएं। `substring` में दो संख्याओं का उपयोग किया जाता है, पहला `start` और दूसरा `end`। स्ट्रिंग के संख्या के लिए, हमारे उदाहरण में हम "हमारे स्ट्रिंग से डेटा निकालें।" के लिए संख्या 3 और 11 का उपयोग करते हैं। ये दो संख्याएं स्ट्रिंग के छोटी बूटी से शुरू और अंत में हैं, और उन सभी चरों को शामिल करेंगे जो दो संख्याओं के बीच हैं। ये नया substring स्ट्रिंग के रूप में दिखाई देता है। 

## देखें भी

 - [MDN - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
 - [W3Schools - Try It Yourself: