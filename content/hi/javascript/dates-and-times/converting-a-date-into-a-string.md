---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases:
- /hi/javascript/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:28.476045-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
तारीख को स्ट्रिंग में बदलने का मतलब है एक तारीख ऑब्जेक्ट को पढ़ने लायक टेक्स्ट फॉर्मेट में परिवर्तित करना। डेवलपर्स इसे यूज़र इंटरफेस में तारीख दिखाने, लॉग फाइल्स में तारीख लिखने, या डेटा को सहेजते और पुनः प्राप्त करने के लिए करते हैं।

## How to:
```Javascript
let currentDateTime = new Date();
console.log(currentDateTime.toString()); // "Wed Mar 24 2021 11:28:34 GMT+0530 (India Standard Time)"

console.log(currentDateTime.toISOString()); // "2021-03-24T05:58:34.959Z"

console.log(currentDateTime.toLocaleDateString('hi-IN')); // "24/3/2021"

console.log(currentDateTime.toTimeString()); // "11:28:34 GMT+0530 (India Standard Time)"
```

## Deep Dive
तारीख को स्ट्रिंग में बदलने के कई तरीके हैं। सबसे पहले, `.toString()` से शुरू होते हैं जो जावास्क्रिप्ट के पुराने वर्ज़न्स से है। यह पूर्ण तारीख और समय दिखाता है। इसके बाद `.toISOString()` है, जो ISO-8601 फॉर्मेट में तारीख देता है - यह डाटाबेस स्टोरेज और API के लिए उपयोगी होता है। फिर `.toLocaleDateString()` है जो लोकल फॉर्मेटिंग के साथ तारीख देता है, अर्थात अलग-अलग भागों में अलग तरीके से तारीख दिखाना। मसलन, 'hi-IN' पारित करने पर, हिंदी (भारत) के लिए स्थानीयकृत तारीख प्राप्त होती है।
`.toTimeString()` सिर्फ समय भाग देता है। ये फंक्शन जावास्क्रिप्ट Date ऑब्जेक्ट की परिचालित परिभाषाओं का हिस्सा हैं और तारीख-संबंधित डेटा को मैनेज करने में विभिन्न परिदृश्यों के लिए उपयुक्त होते हैं।

## See Also
- MDN Web Docs for Date objects: [Date - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- ISO 8601 Date and time format: [ISO 8601 - Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
- Date and time formatting in JavaScript: [Date.prototype.toLocaleDateString() - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
