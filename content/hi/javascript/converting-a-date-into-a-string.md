---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"डेट को स्ट्रिंग में बदलना" एक ऐसी प्रक्रिया है जिसमें हम एक तारीख को पाठ(स्ट्रिंग) रूप में बदलते हैं। प्रोग्रामर्स इसे तब करते हैं जब उन्हें डेटा को उपयोगकर्ता-मित्री तरीके से प्रदर्शित करना होता है या सर्वर से डेटा क्वेरी करना होता है।

## कैसे करें:

आइए देखें कि हम JavaScript में डेट को स्ट्रिंग में कैसे परिवर्तित करते हैं।

```Javascript 
let currentDate = new Date();
let stringDate = currentDate.toString();
console.log(stringDate);
```

यदि आप कोड को चलाते हैं, तो आपको सिस्टम की मौजूदा तारीख और समय मिलेगा, जैसे की "Tue Feb 22 2022 17:10:16 GMT+0530 (India Standard Time)"। 

## गहराई से जानकारी:

JavaScript में, `Date` object को 1970 से ISO 8601 spec के अनुसार डेट और टाइम की गणना की गई है। `toString` मेथड सिस्टम क्षेत्र की settings के आधार पर डेट को स्ट्रिंग में बदलता है। वैकल्पिक रूप से, आप `toLocaleDateString` या `toUTCString` जैसे अन्य मेथडों का उपयोग कर सकते हैं।

यदि आप अधिक अनुकूलित प्रारूप चाहते हैं, तो `Moment.js` जैसे libraries का उपयोग कर सकते हैं। वे विशेष तथा पेशेवर स्तर के फ़ॉर्मैटिंग विकल्प प्रदान करते हैं।

## अधिक देखें:

विस्तृत जानकारी और संबंधित सामग्री के लिए:
- Date object (Mozilla): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/ 
- JavaScript Date Formats (w3schools): https://www.w3schools.com/js/js_date_formats.asp