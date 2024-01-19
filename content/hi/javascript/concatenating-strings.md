---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

श्रृंखला जोड़ना (concatenating) यानी दो या अधिक स्ट्रिंग्स को एक साथ जोड़ना। प्रोग्रामर्स इसे तभी उपयोग करते हैं जब उन्हें विभिन्न स्ट्रिंग्स को एक स्ट्रिंग में जोड़ना होता है।

## कैसे करें:

निम्नलिखित कोड ब्लॉक में जावास्क्रिप्ट का उपयोग करके श्रृंखला जोड़ने का उदाहरण देखें।

```Javascript
let str1 = "नमस्ते ";
let str2 = "दुनिया!";
let result = str1.concat(str2);
console.log(result); // "नमस्ते दुनिया!"
```
## गहराई में:

**ऐतिहासिक संदर्भ** - जावास्क्रिप्ट ने अपने पहले संस्करण में ही श्रृंखला जोड़ने की सुविधा शामिल की थी।

**विकल्प** - आप `+` आपरेटर का भी उपयोग कर सकते हैं, जैसे -

```Javascript
let result = str1 + str2; // "नमस्ते दुनिया!"
```

**कार्यान्वयन विवरण** - `.concat()` मेथड नई स्ट्रिंग्स की नकल (copy) बनाता है, यह मौलिक स्ट्रिंग्स को परिवर्तित नहीं करता। 

## यह भी देखें:

- [MDN Web Docs - श्रृंखला जोड़ना](https://developer.mozilla.org/hi/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [JavaScript Info - Strings](https://javascript.info/string)

> Note: यह मेचिन ट्रांसलेशन है। कृपया मानवीय त्रुटियों माफ करें।