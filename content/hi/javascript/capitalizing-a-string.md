---
title:    "Javascript: स्ट्रिंग को उच्चारित करना"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## क्यों
कोई भी देखता है कि जब हम एक string को capitalize करते हैं तो हम किस प्रकार से उसमें बदलाव ला सकते हैं। यह उपयोगी होता है क्योंकि कई प्रोग्रामिंग परियोजनाओं में, हमें डेटा को एक सिस्टम से दूसरे सिस्टम में ट्रांसफर करना होता है और इसमें स्ट्रिंग का फॉर्मेटिंग सही होना जरूरी होता है। 

## कैसे करें
```
Javascript
let str = "hello world";
console.log(str.toUpperCase());
// Output: HELLO WORLD
```

```
Javascript
let str = "hello world";
console.log(str[0].toUpperCase() + str.slice(1));
// Output: Hello world
```

## गहराई खोज करें
जब हम एक string को capitalize करते हैं तो हमारे पास कई विकल्प होते हैं। उपयुक्त कैसे खोजें और किस तरह से उन्हें समाप्त करें यह गहराई से जानना महत्वपूर्ण है। हम इस ट्यूटोरियल में यह बातें गहराई से समझेंगे ताकि आप अपने प्रोजेक्ट में स्ट्रिंग को ठीक से capitalize कर सकें। 

## देखें भी
- [Javascript में String interpolation कैसे करें](https://www.w3schools.com/js/js_string_interpolation.asp)
- [String methods और उनका इस्तेमाल कैसे करें](https://www.javatpoint.com/javascript-string-methods)