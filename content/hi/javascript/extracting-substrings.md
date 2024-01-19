---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Substring निकालना मतलब है किसी मूल स्ट्रिंग से छोटा भाग (सबस्ट्रिंग) लेना। प्रोग्रामर्स इसे डेटा में पत्तरन्स खोजने, डेटा विश्लेषण और टेक्स्ट मैनिपुलेशन के लिए उपयोग करते हैं।

## कैसे:
Javascript में सबस्ट्रिंग निकालने के लिए मुख्य रूप से तीन फ़ंक्शन्स होते हैं: `substring()`, `substr()` और `slice()`।

```Javascript
var str = "नमस्ते दुनिया!";
// substring() method
console.log(str.substring(0, 5));  // नमस्ते
// substr() method
console.log(str.substr(0, 5));  // नमस्ते
// slice() method
console.log(str.slice(0, 5));  // नमस्ते
```
सभी उपरोक्त फ़ंक्शन्स हमें "नमस्ते" नाम का सबस्ट्रिंग देंगे।

## गहरी डाइव
इन three फ़ंक्शन्स का इस्तेमाल हम किसी स्ट्रिंग में विशेष जानकारी खोजने के लिए करते हैं। इनका इस्तेमाल JavaScript 1.0 से शुरू हुआ था। `substring()` और `substr()` में थोड़ा अंतर होता है -`substring()` द्वितीय पैरामीटर (`end`) का उपयोग अंत स्थान को सूचित करने के लिए करता है, जबकि `substr()` द्वितीय पैरामीटर (`length`) का उपयोग सबस्ट्रिंग की लंबाई को सूचित करता है। `slice()` function का व्यवहार `substring()` के साथ समान होता है, लेकिन वह नकारात्मक indices का भी समर्थन करता है।

## यह भी देखें
1. [MDN का JavaScript सबस्ट्रिंग गाइड](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
2. [W3Schools का सबस्ट्रिंग ट्यूटोरियल](https://www.w3schools.com/js/js_string_methods.asp)