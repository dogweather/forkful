---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:09.478985-07:00
description: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\
  \u0947 \u0921\u0947\u091F \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0915\u094B \u091F\u0947\u0915\u094D\u0938\u094D\u091F\u0941\u0905\u0932 \u0921\
  \u0947\u091F \u092A\u094D\u0930\u0924\u093F\u0928\u093F\u0927\u093F\u0924\u094D\u0935\
  \ \u0915\u094B JavaScript `Date` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\
  \u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948, \u091C\u094B \u0921\u0947\
  \u091F\u2026"
lastmod: '2024-03-13T22:44:53.005777-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947\
  \ \u0921\u0947\u091F \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\
  \u093E \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0915\u094B \u091F\u0947\u0915\u094D\u0938\u094D\u091F\u0941\u0905\u0932 \u0921\
  \u0947\u091F \u092A\u094D\u0930\u0924\u093F\u0928\u093F\u0927\u093F\u0924\u094D\u0935\
  \ \u0915\u094B JavaScript `Date` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\
  \ \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940 \u0905\u0928\u0941\
  \u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948, \u091C\u094B \u0921\u0947\
  \u091F\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
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
