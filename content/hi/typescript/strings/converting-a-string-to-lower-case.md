---
date: 2024-01-20 17:39:45.937989-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:53.877382-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## How to: (कैसे करें:)
```TypeScript
let greeting: string = "नमस्ते दुनिया!";
let lowerCaseGreeting: string = greeting.toLowerCase();

console.log(lowerCaseGreeting); // "नमस्ते दुनिया!" को छोटे अक्षरों में प्रिंट करेगा: "नमस्ते दुनिया!"
```

## Deep Dive (गहराई से जानकारी):
स्ट्रिंग्स को लोअर केस में बदलना अनेक प्रोग्रामिंग भाषाओं में एक बुनियादी कार्य है। यह इंटरनेट की शुरुआत के समय से डेटा को सामंजस्यपूर्ण बनाने के लिए उपयोग में लाया जाता रहा है। TypeScript में `.toLowerCase()` विधि जावास्क्रिप्ट की स्ट्रिंग ऑब्जेक्ट से आती है और यह यूनिकोड मानक का समर्थन करता है, जिससे यह विभिन्न भाषाओं और स्क्रिप्ट्स के लिए काम करता है।

वैकल्पिक तरीके भी हैं, जैसे कस्टम फंक्शन लिखना या रेगुलर एक्सप्रेशन का उपयोग करना, पर `toLowerCase()` सबसे सरल और प्रभावी है। टर्बो पास्कल जैसी पुरानी भाषाओं में भी यही संकल्पना अस्तित्व में थी।

## See Also (अन्य जानकारियां):
- MDN Web Docs on `.toLowerCase()`: [MDN.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- Unicode Standard: [Unicode.org](https://unicode.org/)
- JavaScript string comparison for different locales: [MDN.localeCompare()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/localeCompare)
