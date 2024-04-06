---
date: 2024-01-20 17:32:13.363763-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0924\u093E\u0930\
  \u0940\u0916\u094B\u0902 \u0915\u0940 \u0917\u0923\u0928\u093E JavaScript \u0915\
  \u0947 \u092A\u094D\u0930\u093E\u091A\u0940\u0928 \u0938\u092E\u092F \u0938\u0947\
  \ \u0939\u094B\u0924\u0940 \u0906 \u0930\u0939\u0940 \u0939\u0948 \u0914\u0930 \u092F\
  \u0939 TypeScript \u092E\u0947\u0902 \u092D\u0940 \u0938\u092E\u093E\u0928 \u0939\
  \u0948, \u0915\u094D\u092F\u094B\u0902\u0915\u093F TypeScript JavaScript \u0915\u093E\
  \ \u0939\u0940 \u090F\u0915 \u0938\u0941\u0927\u093E\u0930\u093F\u0924 \u0930\u0942\
  \u092A\u2026"
lastmod: '2024-04-05T21:53:53.916942-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0917\u0923\u0928\
  \u093E JavaScript \u0915\u0947 \u092A\u094D\u0930\u093E\u091A\u0940\u0928 \u0938\
  \u092E\u092F \u0938\u0947 \u0939\u094B\u0924\u0940 \u0906 \u0930\u0939\u0940 \u0939\
  \u0948 \u0914\u0930 \u092F\u0939 TypeScript \u092E\u0947\u0902 \u092D\u0940 \u0938\
  \u092E\u093E\u0928 \u0939\u0948, \u0915\u094D\u092F\u094B\u0902\u0915\u093F TypeScript\
  \ JavaScript \u0915\u093E \u0939\u0940 \u090F\u0915 \u0938\u0941\u0927\u093E\u0930\
  \u093F\u0924 \u0930\u0942\u092A \u0939\u0948\u0964 \u0906\u092A `Date` \u0911\u092C\
  \u094D\u091C\u0947\u0915\u094D\u091F \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0924\u093E\u0930\u0940\u0916\u094B\
  \u0902 \u0915\u094B \u092E\u0948\u0928\u0947\u091C \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u090F\u0915 \u0905\u0932\u094D\u091F\u0930\u0928\
  \u0947\u091F\u093F\u0935 `moment.js` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\
  \u0940 \u0939\u0948, \u092A\u0930 `Date` \u0939\u0932\u094D\u0915\u093E \u0914\u0930\
  \ \u092C\u093F\u0928\u093E \u0915\u093F\u0938\u0940 \u0905\u0924\u093F\u0930\u093F\
  \u0915\u094D\u0924 \u0921\u093F\u092A\u0947\u0902\u0921\u0947\u0902\u0938\u0940\
  \ \u0915\u0947 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0939\u093E\u0932\u093E\
  \u0902\u0915\u093F, \u091F\u093E\u0907\u092E\u091C\u094B\u0928 \u0914\u0930 \u0932\
  \u0940\u092A \u0908\u092F\u0930\u094D\u0938 \u091C\u0948\u0938\u0940 \u091C\u091F\
  \u093F\u0932\u0924\u093E\u0913\u0902 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0924\u0947 \u0938\u092E\u092F, \u0906\u092A\u0915\u094B \u0907\u0928\
  \ \u091C\u091F\u093F\u0932\u0924\u093E\u0913\u0902 \u0915\u094B \u0938\u092E\u091D\
  \u0928\u0947 \u0915\u0940 \u091C\u0930\u0942\u0930\u0924 \u0939\u094B \u0938\u0915\
  \u0924\u0940 \u0939\u0948\u0964."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

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
