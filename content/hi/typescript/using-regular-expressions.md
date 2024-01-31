---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेग्युलर एक्सप्रेशन (Regular Expressions) पैटर्न से टेक्स्ट को ढूंढने और मैनेज करने की एक तकनीक है। प्रोग्रामर्स इसका इस्तेमाल डाटा वैलिडेशन, पार्सिंग या टेक्स्ट रिप्लेसमेन्ट के लिए करते हैं क्योंकि यह शक्तिशाली और फ्लेक्सिबल होता है।

## How to: (कैसे करें:)
```typescript
// TypeScript में RegEx का उपयोग करके ईमेल वैलिडेशन करना:
function validateEmail(email: string): boolean {
  const regex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  return regex.test(email);
}

// ईमेल वैलिड है या नहीं चेक करें:
console.log(validateEmail('example@domain.com')); // true
console.log(validateEmail('wrong-email@')); // false
```

```typescript
// TypeScript में RegEx का उपयोग करके स्ट्रिंग से नंबर्स निकालना:
function extractNumbers(text: string): number[] {
  const regex = /\d+/g;
  const matches = text.match(regex);
  return matches ? matches.map(Number) : [];
}

// परिणाम देखें:
console.log(extractNumbers('abc123def45')); // [123, 45]
console.log(extractNumbers('no numbers')); // []
```

## Deep Dive (गहराई से जानकारी)
रेग्युलर एक्सप्रेशन्स साइंटिस्ट (Scientist) स्टीफेन क्लेन (Stephen Kleene) ने 1950s में बनाए थे। यह तब से प्रोग्रामिंग, टेक्स्ट एडिटिंग और थ्योरेटिकल कंप्यूटर साइंस में एक महत्वपूर्ण उपकरण बन गया है। विकल्प के रूप में पार्सर जनरेटर्स जैसे ANTLR या लेक्सिकल एनालाइज़र होते हैं, लेकिन उनका उपयोग जटिल होता है। TypeScript में, रेग्युलर एक्सप्रेशन्स का इंप्लीमेंटेशन जावास्क्रिप्ट (JavaScript) की तरह ही है क्योंकि TypeScript जावास्क्रिप्ट में ट्रांसपाइल हो जाता है।

## See Also (देखें भी)
- [MDN Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [RegExr: ऑनलाइन RegEx टेस्टर और डीबगर](https://regexr.com/)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
