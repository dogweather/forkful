---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- /hi/typescript/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:45.937989-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलना यह है कि मूल स्ट्रिंग के सभी अक्षरों को छोटे (लोअर केस) में परिवर्तित करना। प्रोग्रामर्स यह इसलिए करते हैं ताकि विभिन्न स्ट्रिंग इनपुट्स की तुलना करते समय बड़े-छोटे अक्षर की समस्या ना हो।

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
