---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:32:13.363763-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

किसी भविष्य या अतीत की तारीख की गणना से मतलब है कि हम एक निश्चित तारीख से पहले या बाद की तारीख को निकालें। प्रोग्रामर्स इसका इस्तेमाल किसी इवेंट्स, डेडलाइंस, या अन्य महत्वपूर्ण समय-संबंधी गणनाओं के लिए करते हैं।

## कैसे करें:

```typescript
// आज की तारीख को प्राप्त करें
const today: Date = new Date();

// 10 दिन बाद की तारीख कैलकुलेट करें
const tenDaysLater: Date = new Date(today);
tenDaysLater.setDate(today.getDate() + 10);

console.log(`10 दिन बाद की तारीख: ${tenDaysLater.toLocaleDateString()}`);

// 5 वर्ष पहले की तारीख कैलकुलेट करें
const fiveYearsEarlier: Date = new Date(today);
fiveYearsEarlier.setFullYear(today.getFullYear() - 5);

console.log(`5 वर्ष पहले की तारीख: ${fiveYearsEarlier.toLocaleDateString()}`);
```

## गहराई से जानकारी:

तारीखों की गणना JavaScript के प्राचीन समय से होती आ रही है और यह TypeScript में भी समान है, क्योंकि TypeScript JavaScript का ही एक सुधारित रूप है। आप `Date` ऑब्जेक्ट का इस्तेमाल करके तारीखों को मैनेज कर सकते हैं। एक अल्टरनेटिव `moment.js` लाइब्रेरी है, पर `Date` हल्का और बिना किसी अतिरिक्त डिपेंडेंसी के होता है। हालांकि, टाइमजोन और लीप ईयर्स जैसी जटिलताओं को हैंडल करते समय, आपको इन जटिलताओं को समझने की जरूरत हो सकती है।

## संबंधित सूत्र:

- मोमेंट जेएस के डॉक्यूमेंटेशन: [Moment.js Docs](https://momentjs.com/docs/)
- MDN Web Docs, JavaScript Date ऑब्जेक्ट पर: [MDN JavaScript Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
