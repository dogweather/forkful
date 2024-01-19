---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
वर्तमान तारीख प्राप्त करना का मतलब है कंप्यूटर की सिस्टम कैलेंडर से चल रही तारीख को पढ़ना। यह उन समयों में महत्वपूर्ण होता है जब प्रोग्रामर को कोई कार्रवाई या समयधारित प्रसंगितता को ट्रैक करना चाहिए।

## कैसे करें:
नीचे दिए गए TypeScript कोड का उपयोग करके हम वर्तमान दिनांक को प्राप्त कर सकते हैं:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

यदि आप इसे चलाते हैं, तो आपको निम्नलिखित आउटपुट मिलेगा:

```TypeScript
Tue Jun 29 20:42:24 UTC+0530 2022
```

## गहरा डाइव:
जावास्क्रिप्ट एवं उसकी सुपरसेट भाषा TypeScript में दिनांक और समय को हैंडल करने के लिए `Date` ऑब्जेक्ट का उपयोग किया जाता है, जिसे 1995 में ECMAScript ने मान्‍यता प्रदान की थी।

वैकल्पिक तरीके में, आप `moment.js` जैसे जावास्क्रिप्ट पुस्तकालयों का उपयोग कर सकते हैं जो आपको अधिक शक्ति और क्षमता प्रदान करते हैं। उदाहरण के लिए, विभिन्न स्थानीयकरण और समय के क्षेत्रों का समर्थन।

`new Date()` का उपयोग करके एक नया दिनांक अवसार निर्माण होता है, जो वर्तमान तिमिंग से जुड़ा होता है। यह बोलिवियन मिलीसेकंड के रूप में 1970 के 1 जनवरी से अब तक के समय की प्राप्ति को स्थापित करता है।

## अधिक जानने के लिए:
यदि आप जावास्क्रिप्ट या TypeScript में दिनांक के बारे में और गहराई से जानना चाहते हैं, तो आप निम्नलिखित लिंक पर जा सकते हैं:

1. [MDN Web Docs: JavaScript Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js library](https://momentjs.com/)
3. [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)