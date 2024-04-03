---
date: 2024-01-20 17:58:52.761001-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A\
  \ \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0915\u093E \u092E\u0924\
  \u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0926\u093F\u090F \u0917\u090F\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0936\u092C\
  \u094D\u0926 \u092F\u093E \u092B\u094D\u0930\u0947\u091C \u0915\u094B \u0922\u0942\
  \u0902\u0922\u0915\u0930 \u0909\u0938\u0947 \u092C\u0926\u0932\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\
  \u0947 \u0907\u0938\u0940\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902 \u0924\u093E\u0915\u093F \u0935\u0947 \u0921\u093E\u091F\u093E \u0915\u094B\
  \ \u0905\u092A\u0921\u0947\u091F \u0915\u0930\u2026"
lastmod: '2024-03-13T22:44:52.965319-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A \u0914\
  \u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0915\u093E \u092E\u0924\u0932\
  \u092C \u0939\u0948 \u0915\u093F\u0938\u0940 \u0926\u093F\u090F \u0917\u090F \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u0936\u092C\u094D\
  \u0926 \u092F\u093E \u092B\u094D\u0930\u0947\u091C \u0915\u094B \u0922\u0942\u0902\
  \u0922\u0915\u0930 \u0909\u0938\u0947 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0947\
  \ \u0907\u0938\u0940\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \ \u0924\u093E\u0915\u093F \u0935\u0947 \u0921\u093E\u091F\u093E \u0915\u094B \u0905\
  \u092A\u0921\u0947\u091F \u0915\u0930 \u0938\u0915\u0947\u0902 \u092F\u093E \u091F\
  \u093E\u0907\u092A\u094B \u0915\u094B \u0920\u0940\u0915 \u0915\u0930 \u0938\u0915\
  \u0947\u0902\u0964."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

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
