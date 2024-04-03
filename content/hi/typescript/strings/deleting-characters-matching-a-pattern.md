---
date: 2024-01-20 17:43:44.803451-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:51.861502-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
```typescript
// TypeScript में पैटर्न से मेल खाते वर्णों को हटाने का उदाहरण

function removeCharacters(str: string, pattern: RegExp): string {
  return str.replace(pattern, '');
}

// उपयोग का उदाहरण
const originalString = "हेलो! कैसे हैं आप?";
const pattern = /[ाेौिक]?/g; 

const cleanedString = removeCharacters(originalString, pattern);
console.log(cleanedString); // हेलो! से हैं आप?
```

## Deep Dive (गहन जानकारी):
वर्णों को हटाना JavaScript और इसके सुपरसेट TypeScript में `replace()` फंक्शन के जरिए किया जाता है। यह ECMAScript के एक पुराने वर्शन में पेश किया गया था। अलग अलग पैटर्न के लिए `RegExp` (रेग्युलर एक्सप्रेशन) का उपयोग होता है। इस तरीके से हम जटिल पैटर्न का भी मिलान करके डेटा से उन्हें हटा सकते हैं।

विकल्प के रूप में, कुछ लाइब्रेरीज भी हैं जैसे कि Lodash जो `_.replace` फंक्शन प्रदान करती हैं। हालांकि, ज्यादातर मामलों में बिल्ट-इन `replace()` ही काफी है।

TypeScript में, यह जरूरी है कि जब हम `RegExp` का उपयोग करें तो हमें स्ट्रिंग के टाइप का ध्यान रखना होता है, इससे टाइप सेफ्टी और कोड की सुसंगतता बनी रहती है।

## See Also (और जानकारी के लिए):
- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExp - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Lodash _.replace Method](https://lodash.com/docs/4.17.15#replace)
