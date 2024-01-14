---
title:                "TypeScript: स्ट्रिंग्स को एक साथ जोड़ना"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों
कोई भी प्रोग्रामिंग भाषा में स्ट्रिंग को जोड़ना एक आम और उपयोगी काम है। स्ट्रिंग को जोड़कर हम अपनी प्रोग्राम में समूहीकरण करते हैं और इससे हमारे कोड को लिखने में आसानी होती है।

## कैसें करें
```TypeScript
let firstString = "हेलो";
let secondString = "दुनिया!";

let concatenation = firstString + secondString;

console.log(concatenation);
```
आप कोड को चलाएंगे तो कॉन्सोल पर हमें `हेलोदुनिया!` मिलेगा। इसमें हमने दो स्ट्रिंग `firstString` और `secondString` को जोड़कर एक नए स्ट्रिंग `concatenation` बनाया है। हम `+` ऑपरेटर का उपयोग करके स्ट्रिंग को जोड़ सकते हैं। उदाहरण के लिए, यदि हम `firstString = "Hello"` और `secondString = "World!"` लिखते तो हमें `HelloWorld!` मिलता।

## गहराई में जाएं
स्ट्रिंग को जोड़ने के लिए, हम अनेक तरह के विधियों का उपयोग कर सकते हैं। उपरोक्त उदाहरण में हमने दो स्ट्रिंग को जोड़ने के लिए `+` ऑपरेटर का उपयोग किया है, लेकिन हम `concat()` फ़ंक्शन या `.concat()` मैथमेटिकल ऑपरेटर से भी स्ट्रिंग को जोड़ सकते हैं।  इसके अलावा स्ट्रिंग टेम्पलेट लिटरेल्स भी उपयोगी हो सकते हैं जहां हम `${}` से भिन्न भिन्न मानों को एक साथ जोड़ सकते हैं। 

## देखें भी
- [टाइपस्क्रिप्ट मैनुअल](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [टाइपस्क्रिप्ट स्ट्रिंग ऑपरेटर्स](https://www.typescriptlang.org/docs/handbook/2/objects.html#string-literal-types)