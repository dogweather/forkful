---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:38:27.732154-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग में बदलना मतलब है, एक तारीख को पढ़ने योग्य फॉर्मेट में बदलना। प्रोग्रामर इसे इसलिए करते हैं क्योंकि यह तारीखों को लॉग करने, दिखाने या अन्य सिस्टम्स के साथ साझा करने में आसानी प्रदान करता है।

## How to: (कैसे करें:)
```TypeScript
let currentDate: Date = new Date();
let dateString: string = currentDate.toISOString();
console.log(dateString); // "2023-05-21T14:00:00.000Z" जैसा आउटपुट देगा

// एक विशिष्ट फॉर्मेट में तारीख प्रारूपण
const options: Intl.DateTimeFormatOptions = {
  year: 'numeric',
  month: 'long',
  day: 'numeric',
  hour: '2-digit',
  minute: '2-digit',
  second: '2-digit',
  timeZoneName: 'short'
};

let formattedDate: string = new Intl.DateTimeFormat('en-US', options).format(currentDate);
console.log(formattedDate); // "May 21, 2023, 02:00:00 PM GMT+5" जैसा आउटपुट देगा
```

## Deep Dive (गहराई में):
तारीख को स्ट्रिंग में बदलने की प्रक्रिया हमेशा से कंप्यूटर प्रोग्रामिंग का हिस्सा रही है क्योंकि हमेशा नए तरीके और मानक आते रहे हैं। ISO 8601 फॉर्मेट एक अंतरराष्ट्रीय मानक है जो विभिन्न सिस्टम्स के बीच साझा करने में मदद करता है। `Date.prototype.toISOString` जैसे मेथड इसी स्टैंडर्ड को सपोर्ट करते हैं। 

हालांकि, जब एक निश्चित फॉर्मेट की आवश्यकता होती है, `Intl.DateTimeFormat` का इस्तेमाल किया जाता है। यह एपीआई लोकेल और ऑप्शन्स पर आधारित तारीख को प्रारूपित करता है।

इसके अलावा `Date.prototype.toLocaleString` या तृतीय-पक्ष लाइब्रेरीज़ जैसे `moment.js`, `date-fns` भी उपलब्ध हैं, जिनसे अधिक रिच तारीख प्रारूपण विकल्प मिलते हैं।

## See Also (यह भी देखें):
- [ECMAScript Internationalization API Specification](https://tc39.es/ecma402/)
- [`moment.js` library](https://momentjs.com/)
- [`date-fns` library](https://date-fns.org/)