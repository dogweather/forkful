---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- /hi/typescript/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:43:44.803451-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न से मिलते वर्णों को हटाना एक प्रोसेस है जिसमें हम एक निर्धारित पैटर्न के अनुसार स्ट्रिंग से वर्णों को हटा देते हैं। कोड में सफाई और डेटा में जरूरी परिवर्तन के लिए प्रोग्रामर्स ऐसा करते हैं।

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
