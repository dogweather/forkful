---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

पैटर्न से मेल करने वाले कैरेक्टर को हटाना, एक स्ट्रिंग में से विशेष कैरेक्टर्स को निकालना होता है। प्रोग्रामर इसे डेटा संग्रहण, प्रसंस्करण और मानकीकरण को सुगम और सटीक बनाने के लिए करते हैं।

## कैसे करें:

आइए JavaScript का `replace()` फ़ंक्शन का उपयोग करके देखते हैं:

```Javascript
let str = "Hello World!";
let newStr = str.replace(/l/g, '');
console.log(newStr); // "Heo Word!";
```
इसमें `/l/g` पैटर्न सभी 'l' कैरेक्टर्स को मैच करता है और उन्हें हटा देता है। 

## गहरी जाँच:

### ऐतिहासिक संदर्भ:

JavaScript में, ऐसे कैरेक्टर हटाने के लिए विभिन्न तरीके हैं, जैसे कि `split()` और `join()` फ़ंक्शंस का उपयोग करना। लेकिन `replace()` फ़ंक्शन सबसे सरल और सटीक होता है। 

### वैकल्पिक:

आप `split()` और `join()` फ़ंक्शन का भी उपयोग कर सकते हैं:

```Javascript
let str = "Hello World!";
let newStr = str.split('l').join('');
console.log(newStr); // "Heo Word!";
```

### क्रियान्वयन विवरण:

`replace()` फ़ंक्शन JavaScript में Regex (Regular Expressions, नियमित अभिव्यक्तियां) का उपयोग करता है करने के लिए। `/l/g` यहां एक regex है, जहां 'l' हमारा लक्षित कैरेक्टर है, और 'g' सभी मिलान करने के लिए ‘global’ का संकेत है। 

## और भी देखें:

- [Mozilla Documentation on replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Mozilla Documentation on Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)