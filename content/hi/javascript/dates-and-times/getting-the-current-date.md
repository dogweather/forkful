---
title:                "वर्तमान तारीख प्राप्त करना"
aliases:
- hi/javascript/getting-the-current-date.md
date:                  2024-02-03T19:10:49.310969-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
JavaScript में वर्तमान दिनांक प्राप्त करना एक मौलिक कार्य है, जिसमें आज की दिनांक और समय को प्राप्त करना और संभवतः उसे संशोधित करना शामिल है। प्रोग्रामर इसे वेबसाइटों पर दिनांकें प्रदर्शित करने, अनुप्रयोगों में, उपयोगकर्ता संवादों को ट्रैक करने या समय-संवेदनशील डेटा को संभालने के लिए करते हैं।

## कैसे करें:
वनिला JavaScript में, `Date` ऑब्जेक्ट का उपयोग दिनांकों और समयों के साथ काम करने के लिए किया जाता है। यहाँ वर्तमान दिनांक और समय कैसे प्राप्त करें:

```javascript
const currentDate = new Date();
console.log(currentDate); // उदाहरण आउटपुट: Fri Apr 14 2023 12:34:56 GMT+0100 (British Summer Time)
```

केवल दिनांक को एक और उपयोगकर्ता-अनुकूल प्रारूप में प्रदर्शित करने के लिए, आप `toLocaleDateString()` जैसी विधियों का उपयोग कर सकते हैं:

```javascript
console.log(currentDate.toLocaleDateString()); // उदाहरण आउटपुट: 4/14/2023
```

प्रारूप पर अधिक नियंत्रण के लिए, *Moment.js* या *date-fns* जैसी थर्ड-पार्टी लाइब्रेरियाँ बहुत लोकप्रिय हैं, हालांकि यह जानना अच्छा है कि Moment.js अब एक विरासती प्रोजेक्ट के रूप में माना जाता है जो मेंटेनेंस मोड में है।

*Moment.js* का उपयोग करते हुए:

```javascript
const moment = require('moment'); // Node.js का अनुमान लगाते हुए या एक मॉड्यूल बंडलर का उपयोग करते हुए
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // उदाहरण आउटपुट: 2023-04-14
```

*date-fns* के साथ, जो मॉड्यूलरीकरण पर जोर देता है जिससे आप केवल जरूरत के अनुसार आयात कर सकते हैं:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // उदाहरण आउटपुट: 2023-04-14
```

प्रत्येक दृष्टिकोण जावास्क्रिप्ट में दिनांकों के साथ काम करने के लिए विभिन्न स्तरों की सुविधा और लचीलापन प्रदान करता है, `Date` ऑब्जेक्ट से लेकर लाइब्रेरियों के माध्यम से उपलब्ध अधिक सुविधाजनक प्रारूपण और संशोधन क्षमताओं तक।
