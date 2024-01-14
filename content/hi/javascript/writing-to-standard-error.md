---
title:    "Javascript: स्टैंडर्ड त्रुटि पर लिखना"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों

जब हम प्रोग्रामिंग में से स्टैंडर्ड एरर को लिखते हैं, तो हमें अपने कोड में होने वाली संभावित गलतियों का पता चलता है और उन्हें सही करने में सक्षम होते हैं।

## कैसे

स्टैंडर्ड एरर को लिखने के लिए, हम आमतौर पर `console.error()` फ़ंक्शन का उपयोग करते हैं। यहां कुछ उदाहरण हैं:

```
// एक चर की दोहरी मूल्यांकन
let num = '5';
console.error(num * 2)

// Output:
// NaN (Not a Number)
```

```
// मानक Devanagari अक्षरों का उपयोग करके 'hello' लिखना
console.error('हैलो')

// Output:
// hello
```

```
// Undeclared चर का उपयोग करना
console.error(nonexistentVariable)

// Output:
// ReferenceError: nonexistentVariable is not defined
```

## गहराई में जाएं

स्टैंडर्ड एरर को लिखने का मुख्य उद्देश्य हमें अपने कोड में होने वाली गलतियों का पता चलाना है। इससे हम अपने कोड को स्थायी रूप से सुधार सकते हैं। यदि हम स्टैंडर्ड एरर का उपयोग न करें, तो हमारे कोड में छिपे हुए गलतियों का पता नहीं चलेगा और बड़ी समस्याओं का सामना करना पड़ सकता है।

## देखे भी

- [console.error() डॉक्यूमेंटेशन](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [जावास्क्रिप्ट लॉजिंग के तरीके](https://www.geeksforgeeks.org/javascript-logging-techniques/)