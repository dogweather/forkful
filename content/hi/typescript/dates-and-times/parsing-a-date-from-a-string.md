---
title:                "स्ट्रिंग से तारीख पार्स करना"
date:                  2024-02-03T19:16:43.889363-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से तारीख पार्स करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से तारीख का पार्सिंग करने का मतलब है तारीखों और समयों के टेक्स्टुअल प्रतिनिधित्वों को ऐसे फॉर्मेट में बदलना जिसे प्रोग्राम द्वारा संशोधित और विश्लेषित किया जा सके। यह प्रोग्रामिंग में एक आम कार्य है क्योंकि इससे उपयोगकर्ता के इनपुट को संभालने, समय-छापित डेटा के संग्रहण, और APIs के साथ अंतरक्रियाओं को संभव बनाती है, जिससे अधिक कार्यात्मक और उपयोगकर्ता-मित्रतापूर्ण अनुप्रयोग बनते हैं।

## कैसे करें:
TypeScript, जो कि JavaScript का एक सुपरसेट है, स्ट्रिंग्स से तारीखों का पार्सिंग करने के लिए Date ऑब्जेक्ट पर निर्भर करता है। हालांकि, JS/TS में तारीखों के साथ काम करना Date ऑब्जेक्ट की विचित्रताओं के कारण वर्णनात्मक या अप्रेसिज हो सकता है। यहां एक बुनियादी उदाहरण दिया गया है उसके बाद `date-fns`, एक लोकप्रिय लाइब्रेरी का उपयोग करने का दृष्टिकोण दिया गया है, जो अधिक मजबूत समाधान प्रदान करता है।

### JavaScript के Date ऑब्जेक्ट का उपयोग करना
```typescript
// Date constructor का उपयोग करके मूल पार्सिंग 
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// GMT के लिए आउटपुट: "Fri Apr 21 2023 15:00:00 GMT+0000 (Coordinated Universal Time)"
```

यह विधि ISO फॉर्मेट स्ट्रिंग्स और कुछ अन्य डेट फॉर्मेट्स के लिए काम करती है, लेकिन विभिन्न ब्राउज़रों और लोकेल्स में अस्पष्ट फॉर्मेट्स के लिए असंगत परिणाम दे सकती है।

### date-fns का उपयोग करना
`date-fns` लाइब्रेरी तारीखों के सीधे और सुसंगत व्यवहार को प्रदान करती है। यह एक मॉड्यूलर लाइब्रेरी है, जो आपको केवल आवश्यक हिस्सों को शामिल करने की अनुमति देती है, बंडल का आकार कम करते हुए।

पहले, `date-fns` इंस्टॉल करें:

```sh
npm install date-fns
```

फिर, इसे एक दिनांक स्ट्रिंग को पार्स करने के लिए उपयोग करें:

```typescript
import { parseISO, format } from 'date-fns';

// एक ISO स्ट्रिंग को पार्स करना
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// तारीख को फॉर्मेट करना (उदाहरण के लिए, मानव-पठनीय रूप में)
console.log(format(parsedDate, "PPPpp")); 
// आउटपुट: "Apr 21st, 2023 at 3:00 PM" (आउटपुट लोकेल के आधार पर भिन्न हो सकता है)
```

`date-fns` विभिन्न यूजर क्षेत्रों में सटीक तारीख पार्सिंग और फॉर्मेटिंग की आवश्यकता वाले अनुप्रयोगों के लिए विभिन्न प्रारूपों और लोकेलों का समर्थन करते हुए एक मजबूत विकल्प बनाता है।