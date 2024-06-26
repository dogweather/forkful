---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:51.350232-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: TypeScript \u092E\
  \u0947\u0902, \u0906\u092A \u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0926\u093F\
  \u0928\u093E\u0902\u0915 \u0914\u0930 \u0938\u092E\u092F \u092A\u094D\u0930\u093E\
  \u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `Date`\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\
  \u0939\u093E\u0901 \u092A\u0930 \u0906\u092A\u0915\u0947 \u0932\u093F\u090F \u0907\
  \u0938\u0947 \u0915\u0930\u0928\u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E\
  \ \u0939\u0948."
lastmod: '2024-03-13T22:44:51.907277-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u092E\u0947\u0902, \u0906\u092A \u0935\u0930\u094D\u0924\u092E\
  \u093E\u0928 \u0926\u093F\u0928\u093E\u0902\u0915 \u0914\u0930 \u0938\u092E\u092F\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F `Date` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u092A\u0930 \u0906\u092A\u0915\u0947\
  \ \u0932\u093F\u090F \u0907\u0938\u0947 \u0915\u0930\u0928\u0947 \u0915\u093E \u0924\
  \u0930\u0940\u0915\u093E \u0939\u0948."
title: "\u0935\u0930\u094D\u0924\u092E\u093E\u0928 \u0924\u093E\u0930\u0940\u0916\
  \ \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E"
weight: 29
---

## कैसे करें:
TypeScript में, आप वर्तमान दिनांक और समय प्राप्त करने के लिए `Date` ऑब्जेक्ट का उपयोग कर सकते हैं। यहाँ पर आपके लिए इसे करने का तरीका है:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

नमूना आउटपुट:
```
2023-04-12T07:20:50.52Z
```

यह कोड स्निपेट वर्तमान दिनांक और समय से युक्त एक नया `Date` ऑब्जेक्ट बनाता है, जिसे फिर कंसोल में प्रिंट किया जाता है। आप अधिक पठनीय प्रारूपों के लिए तिथि को toLocaleDateString() का उपयोग करके भी प्रारूपित कर सकते हैं:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

नमूना आउटपुट:
```
4/12/2023
```

### date-fns का उपयोग करते हुए
अधिक व्यापक दिनांक मैनिपुलेशन और प्रारूपण के लिए, `date-fns` लाइब्रेरी एक लोकप्रिय विकल्प है। पहले, इसे npm के माध्यम से इंस्टाल करें:

```bash
npm install date-fns
```

फिर, आप इसे वर्तमान दिनांक प्रारूपित करने के लिए उपयोग कर सकते हैं:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

नमूना आउटपुट:
```
2023-04-12
```

यह `date-fns` उदाहरण वर्तमान दिनांक को "YYYY-MM-DD" प्रारूप में एक स्ट्रिंग के रूप में प्रारूपित करता है। लाइब्रेरी दिनांक मैनिपुलेशन के लिए अनेक फंक्शन प्रदान करती है, जिससे यह किसी भी TypeScript प्रोग्रामर के लिए दिनांकों के साथ काम करते समय एक बहुमुखी उपकरण बन जाती है।
