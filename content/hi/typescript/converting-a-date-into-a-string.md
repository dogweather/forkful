---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "TypeScript: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

#ये क्या है और क्यों करें?
Converting a date into a string का मतलब है कि हम एक तारीख को एक स्ट्रिंग में रूपांतरित करते हैं| प्रोग्रामर इसे तारीख को अपनी खुद की पसंद अनुसार दिखाने के लिए करते हैं| वह एक स्ट्रिंग में दिन, महीना, साल और समय को अलग-अलग तरीकों से दिखाना पसंद कर सकते हैं|

#कैसे करें:
```TypeScript
const today = new Date(); // तारीख के वर्तमान मूल्य को प्राप्त करता है
const dateString = today.toLocaleDateString("en-US"); // तारीख को स्ट्रिंग में कन्वर्ट करता है
console.log(dateString); // मूल्य को कंसोल में प्रिंट करता है
```
**आउटपुट:** "6/7/2021" (मई के 7, 2021)

#गहराई में जाएँ:
Converting a date into a string के इतिहास की बात करें तो शुरुआत में, प्रोग्रामरों को तारीख को स्ट्रिंग में अनुस्मारक के रूप में रखा गया था| यह आसानी से पढ़ा जा सकने वाली और संग्रह वाले स्थानों को शामिल करने के लिए एक अलग तरीके से संरचित था| हालांकि, TypeScript में यह एक परिचित और आसान प्रक्रिया है जो कि दिन, महीना, साल और समय को बहुत ही समझने योग्य तरीके से डिफ़ॉल्ट स्ट्रिंग में दर्शाता है| यदि आप अपनी तारीख-स्ट्रिंग को अन्य तरीकों से फॉर्मेट करना चाहते हैं, तो JavaScript की Date() फंक्शन के साथ काम करने का एक अल्टर्नेटिव भी मौजूद है|

#इससे जुड़े स्रोत:
- [Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)