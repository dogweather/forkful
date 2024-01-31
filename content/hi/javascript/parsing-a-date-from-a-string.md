---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:37:24.729108-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारीख को स्ट्रिंग से पार्स करना यानी टेक्स्ट फॉर्म में दिये गए डेट को जावास्क्रिप्ट डेट ऑब्जेक्ट में बदलना है। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डेटा को क्रमबद्ध, मान्यता और कैल्कुलेशन के लिए उपयोग कर सकें।

## कैसे करें:
```Javascript
// स्ट्रिंग से तारीख पार्स करने का उदाहरण
const dateString = "2023-03-15";
const parsedDate = new Date(dateString);
console.log(parsedDate); // Wed Mar 15 2023 05:30:00 GMT+0530 (India Standard Time)

// ISO स्ट्रिंग के साथ पार्सिंग
const isoString = "2023-03-15T12:00:00Z";
const parsedISODate = new Date(isoString);
console.log(parsedISODate); // Wed Mar 15 2023 17:30:00 GMT+0530 (India Standard Time)
```

## गहराई से जानकारी:
जावास्क्रिप्ट में तारीख को पार्स करना एक आम काम है, जो ECMAScript 5 से आसान हो गया है, क्योंकि वहां `Date.parse()` और नए `Date` ऑब्जेक्ट के साथ आईएसओ स्टैंडर्ड फॉर्मेट का समर्थन आया था। 

वैकल्पिक तरीकों में `Date` ऑब्जेक्ट के साथ सीधे स्ट्रिंग को पार्स करना, या तारीख पुस्तकालयों जैसे कि Moment.js या Date-fns जैसे टूल्स का उपयोग करना शामिल है। इन लाइब्रेरीज में अधिक सुविधाएं और फॉर्मेट सपोर्ट होते हैं। हमेशा ध्यान रखें कि ब्राउज़रों में तारीख पार्सिंग संगतता के मुद्दे हो सकते हैं। 

जब आप `new Date()` का उपयोग करते हैं, तो जावास्क्रिप्ट इंजन स्ट्रिंग को ISO 8601 फॉर्मेट में पार्स करने का प्रयास करता है। अगर फॉर्मेट मिलान नहीं होता, तो परिणाम `Invalid Date` हो सकता है। 

## इसे भी देखें:
- MDN Web Docs Date reference: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js Documentation: [https://momentjs.com/docs/](https://momentjs.com/docs/)
- Date-fns Library: [https://date-fns.org/](https://date-fns.org/)
- ISO 8601 Date and Time Format: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
