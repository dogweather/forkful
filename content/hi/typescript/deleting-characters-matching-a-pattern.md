---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

कैरैक्टर मैचिंग पैटर्न को हटाना, प्रोग्रामिंग में अक्सर आवश्यक होता है जैसे कि किसी स्ट्रिंग से विशेष कैरैक्टर्स को हटाना। प्रोग्रामर्स इसे करते हैं, क्योंकि कई बार वे अनचाहे कैरैक्टर्स से पाठ को साफ करना चाहते हैं या पाठ को एक विशिष्ट स्वरूप में लाना चाहते हैं।

## कैसे करें:

```TypeScript
let str = 'यहां के सबै हिंदू हँ!';
str = str.replace(/ह/g, '');
console.log(str);
```

उत्तरण का परिणाम:

```TypeScript
'यां के सबै िंदू ँ!'
```

## गहरा डाइव

हिस्टोरिकल कॉन्टेक्स्ट: JavaScript और इसके सुपरसेट TypeScript में, आपको किसी पैटर्न से मेल खाने वाले कैरैक्टर्स को हटाने के लिए 'replace' मेथड का उपयोग करने की आवश्यकता होती है। 

विकल्प: 'split' और 'join' का उपयोग करके भी यह संभव है, लेकिन 'replace' तुरंत और संक्षिप्त विधि है। 

कार्यान्वयन का विवरण: 'replace' मेथड, प्रतिस्थापित होने वाले 'ह' कैरैक्टर्स के लिए ग्लोबल खोज (/g) का उपयोग करके काम करता है। जब यह 'ह' मिलता है, तो इसे खाली स्ट्रिंग द्वारा प्रतिस्थापित कर देता है।

## देखें भी

- [MDN Replace Method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)