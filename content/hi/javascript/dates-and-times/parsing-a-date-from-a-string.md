---
title:                "स्ट्रिंग से तारीख पार्स करना"
aliases:
- hi/javascript/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:09.478985-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से डेट को पार्स करना प्रोग्रामर्स को टेक्स्टुअल डेट प्रतिनिधित्व को JavaScript `Date` ऑब्जेक्ट में बदलने की अनुमति देता है, जो डेट मैनिपुलेशनस, तुलना, और फॉर्मेटिंग ऑपरेशन्स को सुविधाजनक बनाता है। यह प्रक्रिया उपयोगकर्ता इनपुट, डाटाबेस से डेटा प्रोसेसिंग या स्ट्रिंग फॉर्मेट्स में डेट्स को संवाद करनेवाले APIs के साथ काम करने के लिए अत्यावश्यक है।

## कैसे करें:
JavaScript स्वाभाविक रूप से `Date.parse()` मेथड और `Date` कंस्ट्रक्टर प्रदान करता है ताकि डेट स्ट्रिंग को पार्स किया जा सके। हालांकि, इन दृष्टिकोणों में सीमाएँ और विभिन्न ब्राउज़र्स के बीच असंगतियाँ होती हैं, विशेष रूप से गैर-मानक डेट फॉर्मेटों के साथ। इन समस्याओं को संबोधित करने के लिए, `Moment.js` और `date-fns` जैसी तृतीय-पक्ष लाइब्रेरी उनकी रोबस्टता और उपयोगिता की सुविधा के लिए लोकप्रिय हैं।

### देशी JavaScript का उपयोग करते हुए:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // आउटपुट: सन अप्रैल 30 2023 14:55:00 GMT+0000 (Coordinated Universal Time)
```

### Moment.js का उपयोग करते हुए:
सबसे पहले, npm के माध्यम से Moment.js को इंस्टॉल करें या इसे अपने प्रोजेक्ट में शामिल करें। फिर:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // आउटपुट: सन अप्रैल 30 2023 14:55:00 GMT+0000
```

### date-fns का उपयोग करते हुए:
`date-fns` को अपने प्रोजेक्ट में जोड़ने के बाद, एक डेट स्ट्रिंग को इस प्रकार पार्स करें:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // आउटपुट: 2023-04-30T14:55:00.000Z
```

`Moment.js` और `date-fns` दोनों ही विभिन्न फॉर्मैट और लोकेल्स को संभालने सहित अधिक व्यापक पार्सिंग क्षमताएँ प्रदान करते हैं, जो उन्हें जटिल अनुप्रयोगों के लिए पसंदीदा बनाता है।
