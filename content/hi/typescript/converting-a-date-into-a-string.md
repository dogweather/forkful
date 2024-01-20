---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख को एक String में बदलना म्हानता का काम है, जिसे कंप्यूटर programmars use structured data को स्वरूपीत एवं उपयोगी त्रुटि संदेशों,लॉग एंट्रियों, यूआई रेंडरिंग आदि में खोजने के लिए किया जाता है। यह तत्त्व की परतों की व्याख्या करने में आपकी सहायता करता है, इसलिए इसका उपयोग अनिवार्य रूप से किया जाता है। 

## कैसे:

"Date" और "toISOString()" का उपयोग करके TypeScript में दिनांक को स्ट्रिंग में बदलने के लिए कोड कैसे दिखाया जाएगा, उसका उदाहरण नीचे दिया गया है:

``` TypeScript 
let date: Date = new Date(); // आज की तारीख
let str: string = date.toISOString(); // तारीख को स्ट्रिंग में बदलें

console.log(str); // "2021-12-22T12:33:55.629Z"
```

## गहरी जानकारी:

दिनांक को स्ट्रिंग में बदलने का काम Unix एरा के दौरान शुरू हुआ, जिसमें वेब एप्लिकेशन की सहायता के लिए पहली बार इसका उपयोग किया गया था। इसमें वैकल्पिक तरीके शामिल हैं जैसे कि "Date.prototype.toString()" और "Date.prototype.toLocaleString()", जो अलग-अलग स्थितियों में उपयोगी हो सकते हैं। इसका implementation अभ्यास में ब्राउज़र में मानक्क्त होने के आधार पर JavaScript / TypeScript कोड में अद्वितीय होता है।  

## अन्य स्रोत:

- [MDN वेब डॉक्यूमेंटेशन - तारीख वाली ऑब्जेक्ट](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript का अफ़ीशियल डॉक्यूमेंटेशन](https://www.typescriptlang.org/docs/handbook/basic-types.html#about-number)
- [Stack Overflow डिस्कसन टॉपिक ](https://stackoverflow.com/questions/3552461/how-to-format-a-javascript-date)