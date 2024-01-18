---
title:                "स्ट्रिंग से दिनांक खोजना"
html_title:           "Javascript: स्ट्रिंग से दिनांक खोजना"
simple_title:         "स्ट्रिंग से दिनांक खोजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जब हम तारीख को एक स्ट्रिंग से पार्स करते हैं, तो हम स्ट्रिंग से तारीख निकालकर उसे अलग-अलग पार्ट में विभाजित करते हैं। किसी भी प्रोग्रामर के लिए यह एक उपयोगी कौशल है क्योंकि यह उन्हें उस स्ट्रिंग में से दिनांक को सेपरेट करने की परेशानी से छुटकारा देता है।

## कैसे करें:
पार्सिंग एक तारीख के लिए एक स्ट्रिंग में आसानी से कर सकते हैं। नीचे दिए गए कोड ब्लॉक में उदाहरण और साथ ही साथ उनका आउटपुट है:

```Javascript
// एक स्ट्रिंग से तारीख पार्स करने का उदाहरण
const str = '27 September 2021';
const date = new Date(str); // स्ट्रिंग को तारीख में परिवर्तित करने के लिए Date() का उपयोग किया गया है
console.log(date); // Output: Mon Sep 27 2021 00:00:00 GMT+0530 (India Standard Time)
```

```Javascript
// स्ट्रिंग का अलग-अलग अंगों में तारीख पार्स करने का उदाहरण
const str = '04/16/1998'; // MM/DD/YYYY फॉर्मेट में स्ट्रिंग
const date = new Date(str); // स्ट्रिंग को Date() में परिवर्तित करने के लिए
console.log(date); // Output: Fri Apr 16 1998 00:00:00 GMT+0530 (India Standard Time)
const month = date.getMonth() + 1; // अंग्रेजी माह को हिन्दी माह में परिवर्तित करने के लिए
console.log(`माह: ${month}`); // Output: माह: 4
```

## गहराई में जाएं:
पार्सिंग का और भी गहराई से जाना आपको और ज्यादा उपयोगी बना सकता है। इतिहास में, पहले प्रोग्रामर्स को स्ट्रिंग से तारीख परिवर्तन करने के लिए काफी मुसीबत का सामना करना पड़ता था। पर अब स्ट्रिंग को तारीख में परिवर्तित करने के लिए कई लाइब्रेरी उपलब्ध हैं जैसे Moment.js और date-fns जो की आसानी से उपयोग के साथ कई और तारीख प्रक्रियाओं को भी समर्थित करते हैं।

## और भी देखें:
- [JavaScript Date प्रोपर्टी](https://www.w3schools.com/jsref/jsref_date.asp)
- [Moment.js लाइब्रेरी](https://momentjs.com/)
- [date-fns लाइब्रेरी](https://date-fns.org/)