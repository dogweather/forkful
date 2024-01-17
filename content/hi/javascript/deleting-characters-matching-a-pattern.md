---
title:                "एक पैटर्न से मेल खाने वाले अक्षरों को हटाना"
html_title:           "Javascript: एक पैटर्न से मेल खाने वाले अक्षरों को हटाना"
simple_title:         "एक पैटर्न से मेल खाने वाले अक्षरों को हटाना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## आवश्यकता और क्यों?

कम संख्या में शब्दों में, एक पैटर्न को मिलते हुए चरित्रों को हटाने का अर्थ है कि एक प्रोग्रामर को एक चर पैटर्न को खोजने और उसे हटाने के लिए आसान तरीके की सामग्री की खोज करनी होगी। पैटर्न डिलीट करने से प्रोग्राम सुधार और ईफेस करते हुए संरचनाओं को आसानी से संपादित किया जा सकता है, जिससे उपयोगकर्ता और कोडर दोनों के लिए सुधार दर्शाया जा सकता है।

## कैसे करें:

```Javascript
// पहला उदाहरण: एक वाक्य से अक्षरों को हटाएं, "a" को दूसरी वाक्य के आधार पर।
const sentence1 = "My name is Alisha.";
const sentence2 = "Alisha loves to code.";

let newSentence = sentence1.replace(/a/g, sentence2); // पैटर्न को हटाने के लिए "replace" का प्रयोग करें।
console.log(newSentence); // प्रिंट आऊट: "My nme is loves to codebish."
// यहां देखें कि "Alisha" के आधार पर "a" को हटाया जाता है।

// दूसरा उदाहरण: एक स्ट्रिंग से सभी पंत्रों को हटाएं, लेकिन अंक और अंकों के बाद बचें। 
const str = "123Hello456World789";
let newStr = str.replace(/[a-zA-Z]/g, ""); // सभी अक्षरों को हटाने के लिए "/[a-zA-Z]/g" का प्रयोग करें।
console.log(newStr); // प्रिंट आऊट: "123456789"
// यहां देखें कि सभी अक्षरों को हटाया गया है, लेकिन अंकों को छोड़ा गया है।
```

## गहराई में जाएं:

इतिहास की बात करें, तो पैटर्न को हटाने की जरूरत प्रोग्रामिंग भाषाओं के औजार के रूप में प्रेस्ट होती है। वर्तमान में, यह एक महत्वपूर्ण क्रिया उपयोगकर्ताओं को उनके वेब साइटों को उत्तम तरीके से संपादित करने की अनुमति देती है। अतिरिक्त विकल्पों के रूप में, प्रोग्रामर अन्य भाषाओं जैसे Python, C++, और Java में भी "delete characters matching a pattern" का उपयोग कर सकते हैं। पैटर्न को हटाने के लिए, कुछ भाषाओं में "delete", "replace", "cut", और "remove" जैसे चयन के लिए कई तरीके हैं।

## जुड़े रहिए:

अधिक जानकारी के लिए, निम्नलिखित स्रोतों को देखें।

- [JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Regex Tutorial - Regular Expressions in JavaScript](https://www.youtube.com/watch?v=rhzKDrUiJVk)
- [10 Regular Expressions Tips and Tricks in JavaScript](https://www.freecodecamp.org/news/regular-expressions-in-javascript/#how-to-delete-characters-matching-a-pattern)