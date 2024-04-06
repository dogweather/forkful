---
date: 2024-01-20 17:43:44.803451-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0935\u0930\
  \u094D\u0923\u094B\u0902 \u0915\u094B \u0939\u091F\u093E\u0928\u093E JavaScript\
  \ \u0914\u0930 \u0907\u0938\u0915\u0947 \u0938\u0941\u092A\u0930\u0938\u0947\u091F\
  \ TypeScript \u092E\u0947\u0902 `replace()` \u092B\u0902\u0915\u094D\u0936\u0928\
  \ \u0915\u0947 \u091C\u0930\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939 ECMAScript \u0915\u0947 \u090F\u0915 \u092A\
  \u0941\u0930\u093E\u0928\u0947 \u0935\u0930\u094D\u0936\u0928\u2026"
lastmod: '2024-04-05T22:51:06.534974-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0935\u0930\u094D\u0923\
  \u094B\u0902 \u0915\u094B \u0939\u091F\u093E\u0928\u093E JavaScript \u0914\u0930\
  \ \u0907\u0938\u0915\u0947 \u0938\u0941\u092A\u0930\u0938\u0947\u091F TypeScript\
  \ \u092E\u0947\u0902 `replace()` \u092B\u0902\u0915\u094D\u0936\u0928 \u0915\u0947\
  \ \u091C\u0930\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939 ECMAScript \u0915\u0947 \u090F\u0915 \u092A\u0941\u0930\
  \u093E\u0928\u0947 \u0935\u0930\u094D\u0936\u0928 \u092E\u0947\u0902 \u092A\u0947\
  \u0936 \u0915\u093F\u092F\u093E \u0917\u092F\u093E \u0925\u093E\u0964 \u0905\u0932\
  \u0917 \u0905\u0932\u0917 \u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u0947 \u0932\
  \u093F\u090F `RegExp` (\u0930\u0947\u0917\u094D\u092F\u0941\u0932\u0930 \u090F\u0915\
  \u094D\u0938\u092A\u094D\u0930\u0947\u0936\u0928) \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0907\u0938 \u0924\u0930\
  \u0940\u0915\u0947 \u0938\u0947 \u0939\u092E \u091C\u091F\u093F\u0932 \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u0915\u093E \u092D\u0940 \u092E\u093F\u0932\u093E\u0928\
  \ \u0915\u0930\u0915\u0947 \u0921\u0947\u091F\u093E \u0938\u0947 \u0909\u0928\u094D\
  \u0939\u0947\u0902 \u0939\u091F\u093E \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u0935\u093F\u0915\u0932\u094D\u092A \u0915\u0947 \u0930\u0942\u092A \u092E\
  \u0947\u0902, \u0915\u0941\u091B \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\
  \u0940\u091C \u092D\u0940 \u0939\u0948\u0902 \u091C\u0948\u0938\u0947 \u0915\u093F\
  \ Lodash \u091C\u094B `_.replace` \u092B\u0902\u0915\u094D\u0936\u0928 \u092A\u094D\
  \u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u0940 \u0939\u0948\u0902\u0964 \u0939\
  \u093E\u0932\u093E\u0902\u0915\u093F, \u091C\u094D\u092F\u093E\u0926\u093E\u0924\
  \u0930 \u092E\u093E\u092E\u0932\u094B\u0902 \u092E\u0947\u0902 \u092C\u093F\u0932\
  \u094D\u091F-\u0907\u0928 `replace()` \u0939\u0940 \u0915\u093E\u092B\u0940 \u0939\
  \u0948\u0964 TypeScript \u092E\u0947\u0902, \u092F\u0939 \u091C\u0930\u0942\u0930\
  \u0940 \u0939\u0948 \u0915\u093F \u091C\u092C \u0939\u092E `RegExp` \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902 \u0924\u094B \u0939\u092E\
  \u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u091F\
  \u093E\u0907\u092A \u0915\u093E \u0927\u094D\u092F\u093E\u0928 \u0930\u0916\u0928\
  \u093E \u0939\u094B\u0924\u093E \u0939\u0948, \u0907\u0938\u0938\u0947 \u091F\u093E\
  \u0907\u092A \u0938\u0947\u092B\u094D\u091F\u0940 \u0914\u0930 \u0915\u094B\u0921\
  \ \u0915\u0940 \u0938\u0941\u0938\u0902\u0917\u0924\u0924\u093E \u092C\u0928\u0940\
  \ \u0930\u0939\u0924\u0940 \u0939\u0948\u0964."
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
