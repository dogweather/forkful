---
title:                "वर्तमान तारीख प्राप्त करना"
aliases: - /hi/typescript/getting-the-current-date.md
date:                  2024-02-03T19:11:51.350232-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
TypeScript में वर्तमान दिनांक प्राप्त करना, जो JavaScript पर आधारित एक भाषा है, आपको वर्तमान दिनांक और समय की जानकारी तक पहुँचने और उसे मैनिपुलेट करने की अनुमति देता है। प्रोग्रामर्स को अक्सर अपने अनुप्रयोगों में समय संबंधित फीचर्स जैसे कि टाइमस्टैम्प्स बनाने, शेड्यूलिंग, और अन्य समय-संवेदनशील सुविधाओं के लिए इस कार्यक्षमता की आवश्यकता होती है।

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
