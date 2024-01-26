---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:39:01.415443-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारिखें अक्सर स्ट्रिंग के रूप में आती हैं। तारिख को पार्स करने का मतलब है इस स्ट्रिंग को ऐसे फॉर्मेट में बदलना जिसे कंप्यूटर समझ सके। प्रोग्रामर्स इसे तारिखों के साथ गणना और मान्यताएँ करने के लिए करते हैं।

## How to: (कैसे करें)
```typescript
let dateString: string = '2023-04-05T14:48:00.000Z';

// साधारण तरीके से Date ऑब्जेक्ट बनाना
let date: Date = new Date(dateString);
console.log(date); // Wed Apr 05 2023 20:18:00 GMT+0530 (India Standard Time)

// Date-fns लाइब्रेरी का यूज करके पार्स करना
import { parseISO } from 'date-fns';
let parsedDate = parseISO(dateString);
console.log(parsedDate); // Wed Apr 05 2023 20:18:00 GMT+0530 (India Standard Time)
```

## Deep Dive (गहराई से जानकारी)
तारिख पार्स करना ऑनलाइन फॉर्म्स और JSON डेटा के साथ बहुत आम है। इतिहास में, लोग `Date.parse()` या `new Date()` जैसे बिल्ट-इन जावास्क्रिप्ट फ़ंक्शन का यूज करते थे। लेकिन, ये अलग-अलग ब्राउज़र्स में भिन्नताओं की वजह से समस्या पैदा कर सकते हैं।

लाइब्रेरीज़ जैसे `date-fns` या `Moment.js` ने इसे आसान बना दिया है, ये विश्वसनीय पार्सिंग प्रोवाइड करते हैं और टाइमज़ोन्स, लीप सेकंड्स जैसी चीजों को हैंडल भी करते हैं। `date-fns` छोटा और modular है, इसलिए ये पसंद किया जाता है जब आपको पूरी `Moment.js` लाइब्रेरी की जरुरत नहीं होती।

## See Also (और भी देखें)
- MDN Web Docs of Date: [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Date-fns documentation: [date-fns Documentation](https://date-fns.org/v2.16.1/docs/Getting-Started)
- Moment.js Guide: [Moment.js - Docs](https://momentjs.com/docs/)
