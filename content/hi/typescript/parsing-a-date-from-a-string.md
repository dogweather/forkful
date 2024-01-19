---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# डेट पार्सिंग: TypeScript में एक स्ट्रिंग से डेट पार्स करना 

## क्या और क्यों?
डेट पार्सिंग से हमारा मतलब होता है की हम कोई स्ट्रिंग से डेट को निकाल रहे हैं। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे डेटा को सुविधाजनक रूप से उपयोग कर सकते हैं और अनुप्रयोग की आवश्यकताओं को पूरा कर सकते हैं।

## कैसे करें:
TypeScript में डेट पार्सिंग हम निम्नलिखित तरीके से कर सकते हैं।

```typescript
// निम्न स्ट्रिंग से डेट पार्स करें: "2022-04-15"
let parsedDate: Date = new Date("2022-04-15");
console.log(parsedDate); // Outputs: 2022-04-15T00:00:00.000Z
```
यहां पर, `new Date()` इस स्ट्रिंग को `Date` ऑब्जेक्ट में परिवर्तित कर रहा है और हम इसे `parsedDate` में संग्रहित कर रहे हैं।


## गहराई से जानकारी 

डेट पार्सिंग का इतिहास सरल जीवन की शुरुआत से विशिष्ट किया जा सकता है जहां कंप्यूटर उद्योग में तारीख/समय सहनशीलता की आवश्यकता शुरू हुई।

विकल्प रूप में, हम भी `Date.parse()` का उपयोग कर सकते हैं, यदि हम केवल मिलिसेकंड की अवधि प्राप्त करने में रुचि रखते हैं।
```typescript
let timeStamp: number = Date.parse("2022-04-15");
console.log(timeStamp); // Outputs: 1652678400000
```
अंतिम में, इसे कैसे किया जाता है? `Date` कन्स्ट्रक्टर या `Date.parse` विधि ECMAScript कानूनों का अनुसरण करती है, जो कि विभिन्न तारीख और समय स्वरूपों को स्वीकार करती है । 

## अधिक जानकारी के लिए:
1. [MDN Web Docs: Date.parse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
2. [TypeScript: Date Object](https://www.typescripttutorial.net/typescript-tutorial/typescript-date-object/)
3. [ECMAScript 2021 Language Specification](https://tc39.es/ecma262/#sec-date.parse)