---
title:    "TypeScript: एक तारीख को स्ट्रिंग में रूपांतरण करना"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी, हमें अपनी जानकारी को किसी और भाषा में स्पष्ट रूप से प्रदर्शित करने की आवश्यकता हो सकती है। इस मामले में, हम एक तिथि को एक स्ट्रिंग में रूपांतरित करने के लिए TypeScript का उपयोग कर सकते हैं। यह एक दो पद के समान काम हो सकता है, लेकिन इससे हमें प्रोग्रामिंग की अधिक संपदा मिल सकती है।

## कैसे

```TypeScript
let currentDate = new Date();
let dateString = currentDate.toDateString();
console.log(dateString);
```

आउटपुट:
```bash
Mon Jul 26 2021
```

सबसे पहले, हमने `Date` ऑब्जेक्ट का उपयोग करके एक नई तिथि बनाई है। यही तिथि वर्तमान दिनांक और समय होगा। फिर, हमने `toDateString()` फंक्शन को उपयोग करके उस तिथि को एक स्ट्रिंग में रूपांतरित किया है। अंत में, हमने `console.log()` का उपयोग करके उस स्ट्रिंग को प्रिंट कर दिया है।

## गहराई में जाएं

अब आपको शायद यह जानने की आवश्यकता हो कि यह तिथि का स्ट्रिंग में रूपांतरण कैसे होता है। `toDateString()` फंक्शन तिथि ऑब्जेक्ट को एक यूनिक तारीख वाली एक ऑबेक्ट मिलत्या है, जो साल, महीने और दिन को शामिल करता है। हम फंक्शन के विज्ञापन को देख सकते हैं ताकि इसके द्वारा एक यूनिक स्ट्रिंग रिटर्न किया जाए।

## देखें भी

- [Date ऑब्जेक्ट in TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#the-date-type)
- [toDateString() फंक्शन की विवरण](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [अन्य तारीख रूपांतरण चिन्हों के बारे म