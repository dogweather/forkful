---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- /hi/javascript/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:41.664690-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

JavaScript में भविष्य या अतीत की तारीख की गणना करना सरल है। इसकी जरूरत छुट्टियों की योजना, समय-सीमा तय करने, या पुरानी फाइलों के रिकार्ड्‌स को संभालने में पड़ सकती है। 

## कैसे करें:

```Javascript
// आज की तिथि
const today = new Date();

// 10 दिन बाद की तिथि
const tenDaysLater = new Date(today);
tenDaysLater.setDate(tenDaysLater.getDate() + 10);
console.log(tenDaysLater.toString()); // "Sun Apr 10 2023 12:00:00 GMT+0530 (India Standard Time)"

// 30 दिन पहले की तिथि
const thirtyDaysAgo = new Date(today);
thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);
console.log(thirtyDaysAgo.toString()); // "Thu Mar 02 2023 12:00:00 GMT+0530 (India Standard Time)"
```

## गहराई से जानकारी:

तिथियों की गणना के लिए JavaScript में `Date` ऑब्जेक्ट शुरू से ही रहा है, लेकिन समय के साथ इसमें सुधार होते रहे हैं। अन्य तरीकों में, Moment.js और Date-fns जैसे लाइब्रेरीज शामिल हैं, लेकिन ज्यादा सरल गणना के लिए आमतौर पर इनकी जरूरत नहीं पड़ती। जब आप `setDate()` मेथड का उपयोग करते हैं, तो JavaScript खुद ही महीने और वर्ष को संभाल लेता है, अगर तारीखें उनकी सीमा को पार कर जाती हैं। यह समझने के लिए कि JavaScript किस तरह तारीखों को संभालता है, MDN Web Docs का उपयोग कर सकते हैं। 

## संबंधित स्रोत:

- MDN Web Docs on JavaScript Dates: [Mozilla MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- JavaScript Info on Date and Time: [JavaScript.info](https://javascript.info/date)
- Moment.js Library: [Moment.js](https://momentjs.com/)
- Date-fns Library: [Date-fns](https://date-fns.org/)
