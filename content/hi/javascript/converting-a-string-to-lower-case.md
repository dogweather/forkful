---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग को लोअर केस में कन्वर्ट करना मतलब उसके सभी अक्षरों को छोटे अक्षर में परिवर्तित करना। प्रोग्रामर्स इसे करते हैं ताकि आनेवाली इनपुट को संवेदनशीलता से मुक्त करने के लिए, जैसे कि खोज और तुलना कार्यकित्रियाँ।

## कैसे करें:

```Javascript
let str = 'Hello World!';
// Convert it to lower case using toLowerCase() function
let lowerCaseStr = str.toLowerCase();
console.log(lowerCaseStr);  // prints: 'hello world!'
```

इस उदाहरण में, हमने `toLowerCase()` फ़ंक्शन का उपयोग करके एक स्ट्रिंग को लोअर केस में कन्वर्ट किया है।

## गहराई से समझना:

- ऐतिहासिक प्रसंग:
  `toLowerCase()` फ़ंक्शन का उपयोग सेPएकाडिश JavaScript version में संभव हुआ। इसे स्ट्रिंग प्रोटोटाइप में जोड़ा गया था, जिसका मतलब है कि हम इसे किसी भी स्ट्रिंग पर लागू कर सकते हैं। 

- विकल्प:
  `toLowerCase()` का प्रमुख वैकल्पिक उपयोग `toLocaleLowerCase()` है, जो क्षेत्रीय नियमों (locale rules) को मानता है। अगर आपको क्षेत्रीय नियम का समर्थन करना है, तो यह एक बेहतर विकल्प हो सकता है। 

- कार्यान्वयन विवरण:
  `toLowerCase()` function का कार्यान्वयन पट्टी पर नमूने को संग्रहीत करने और नया स्ट्रिंग बनाने के लिए इतरेशन का उपयोग करता है। 

## भी देखें:

- [Mozilla डेवलपर नेटवर्क गाइड](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase) पर `toLowerCase()` function का विवरण।
- [W3Schools ट्यूटोरियल](https://www.w3schools.com/jsref/jsref_tolowercase.asp) जो इस विषय को और अधिक विस्तार से लेकर आती है।