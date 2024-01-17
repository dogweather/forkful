---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "TypeScript: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोडिंग में, दक्षता और स्टाइल को सुरक्षित बनाने के लिए, हम पूर्ण स्ट्रिंग को अपरकेस में लिखते हैं। यह भावुकतापूर्ण होने के अलावा, प्रोग्रामर्स अपने कोड को साफ और स्पष्ट लिखने के लिए भी स्ट्रिंग को अपरकेस में रखते हैं।

## कैसे:
```TypeScript
let greeting: string = "hello world";
console.log(greeting.toUpperCase());
```
आपको अपनी स्ट्रिंग की मान द्वारा `toUpperCase()` फ़ंक्शन को कॉल करके अपरकेस में उसे बदलना होगा। यदि आप आउटपुट के रूप में "HELLO WORLD" देखना चाहते हैं, तो आपको इस तरह से कोडिंग करनी चाहिए।

```TypeScript
let lowercaseString: string = "fOO BAR";
let uppercaseString: string = lowercaseString.toUpperCase();
console.log(uppercaseString);
```
इस तरह, आप एक स्ट्रिंग को पहले लोअरकेस में बनाएं और फिर उसे अपरकेस में बदलें। आप वर्तमान मान की जगह अलग स्ट्रिंग भी प्रयोग कर सकते हैं। जो कि आपको `toUpperCase()` फ़ंक्शन द्वारा लौटाई जाएगी।

## गहराई में खोजें:
पहली स्थिति में, कोडिंग के प्रारंभ के समय स्ट्रिंग को अपरकेस में लिखने की आवश्यकता पैदा हुई थी। नए उपकरण और लॅंग्वेजों ने इसे सरल बनाने का प्रयास किया है, किन्तु अपरकेस में स्ट्रिंग लिखना आज भी काफी लोकप्रिय है। यदि आप इसे सामान्य स्ट्रिंग की तरह ही लिखना चाहते हैं, तो आप `toUpperCase()` को स्ट्रिंग क्लास के साथ कॉल करके बदली हुई स्ट्रिंग को प्राप्त कर सकते हैं। इस तरह से, आपको अन्य चिरप्रतीक या तरीकों की तुलना में स्ट्रिंग को अपरकेस में लिखने के लिए कम प्रोग्राम कोड का उपयोग करना होगा।

## इससे जुड़े लिंक:
- अपनी स्ट्रिंग को अपरकेस में बदलने के लिए [MDN डॉक्यूमेंटेशन](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [TutorialsPoint](https://www.tutorialspoint.com/typescript/typescript_string_to_uppercase.htm) पर स्ट्रिंग को अपरकेस में कैसे बदलें