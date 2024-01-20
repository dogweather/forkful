---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:17:05.877927-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

करेंट डेट पाना यानी मौजूदा दिनांक और समय की जानकारी हासिल करना। प्रोग्रामर्स इसे लॉग्स, टाइमस्टैम्प्स और समय-संबंधित फंक्शनलिटी के लिए करते हैं।

## कैसे करें:

```TypeScript
// करेंट डेट और टाइम पाने के लिए:
const now = new Date();
console.log(now);

// आउटपुट आपके सिस्टम के टाइमज़ोन के अनुसार होगा, जैसे:
// 2023-03-19T05:24:00.000Z
```

## गहराई में जानकारी:

जावास्क्रिप्ट और TypeScript में `Date` ऑब्जेक्ट 1970 से है। यह यूनिक्स टाइम (एपोक) से समय को मिलीसेकंड्स में मापता है। विकल्पों में `moment.js`, `date-fns`, या `Luxon` जैसी लाइब्रेरीज हैं जो अतिरिक्त सुविधाएँ देते हैं। TypeScript में तारीखों को हैंडल करना बेसिक जावास्क्रिप्ट के समान है, यह सिर्फ टाइप सेफ्टी और डेवलपर टूल्स में मदद करता है।

## यह भी देखें:

- MDN Web Docs पर [Date() कंस्ट्रक्टर](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js](https://momentjs.com/)
- [date-fns](https://date-fns.org/)
- [Luxon](https://moment.github.io/luxon/)