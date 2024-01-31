---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:58:52.761001-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट सर्च और रिप्लेस का मतलब है किसी दिए गए स्ट्रिंग में शब्द या फ्रेज को ढूंढकर उसे बदलना। प्रोग्रामर्स ये इसीलिए करते हैं ताकि वे डाटा को अपडेट कर सकें या टाइपो को ठीक कर सकें।

## How to: (कैसे करें:)
```javascript
// सरल उदाहरण: स्ट्रिंग में टेक्स्ट रिप्लेस करना
let text = "Hello, World!";
let newText = text.replace("World", "JavaScript");
console.log(newText); // "Hello, JavaScript!"

// ग्लोबल रिप्लेस उदाहरण: स्ट्रिंग में सभी इंस्टेंस को रिप्लेस करना
let greeting = "नमस्ते दुनिया! दुनिया की सुंदर सुबह!";
let newGreeting = greeting.replace(/दुनिया/g, "विश्व");
console.log(newGreeting); // "नमस्ते विश्व! विश्व की सुंदर सुबह!"
```

## Deep Dive (गहराई में जानकारी):
सर्च और रिप्लेस की क्षमता 1940s के टेक्स्ट एडिटर्स से ही हमारे संग है। Javascript में `replace()` मेथड दो चीज़ों को लेता है - पैटर्न और रिप्लेसमेंट स्ट्रिंग। पैटर्न एक स्ट्रिंग या एक रेगुलर एक्सप्रेशन (RegExp) हो सकता है। अगर पैटर्न एक सिंपल स्ट्रिंग है, तो सिर्फ पहला मैच रिप्लेस होगा। 'g' फ्लैग का उपयोग करके आप ग्लोबली सभी मैचेस को रिप्लेस कर सकते हैं। `replace()` फंक्शन के द्वारा आप कस्टम लॉजिक भी लागू कर सकते हैं अगर आपको रिप्लेसमेंट स्ट्रिंग जेनरेट करने के लिए एक फंक्शन पास करते हैं।

## See Also (और भी जानकारी):
- MDN Web Docs on `replace()`: [MDN replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Regular Expressions Guide: [RegExp Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- JavaScript Info – Replacing the part of the string: [JavaScript Info String Replace](https://javascript.info/string#replacing-the-part-of-the-string)
