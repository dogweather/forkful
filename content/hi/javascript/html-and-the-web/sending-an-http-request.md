---
date: 2024-01-20 18:00:25.229883-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u091C\u093E\
  \u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u092E\u0947\u0902\
  \ HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `fetch` API \u090F\u0915 \u0906\u092E \u0924\u0930\u0940\
  \u0915\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u090F\u0915 \u0938\u093E\
  \u0927\u093E\u0930\u0923 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-04-05T21:53:54.932317-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\
  \u094D\u091F \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\
  \u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `fetch` API \u090F\u0915\
  \ \u0906\u092E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\
  \u0902 \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0909\u0926\u093E\u0939\
  \u0930\u0923 \u0939\u0948."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## कैसे करें? (How to:)
जावास्क्रिप्ट में HTTP अनुरोध भेजने के लिए `fetch` API एक आम तरीका है। यहां एक साधारण उदाहरण है:

```Javascript
fetch('https://jsonplaceholder.typicode.com/posts')
  .then(response => {
    // सफलता की स्थिति में
    if (response.ok) {
      return response.json(); // JSON में परिवर्तित करें
    }
    throw new Error('Network response was not ok.');
  })
  .then(data => console.log(data)) // डेटा दिखाएं
  .catch(error => console.error('Fetch error:', error)); // त्रुटि संभालें
```

सैंपल आउटपुट:

```Javascript
// यह सर्वर से प्राप्त डेटा का एक हिस्सा है
[
  {
    userId: 1,
    id: 1,
    title: "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
    body: "quia et suscipit suscipit recusandae..."
  },
  // ...और भी ऑब्जेक्ट्स
]
```

## गहराई से जानकारी (Deep Dive):
HTTP अनुरोध भेजने के लिए पुराने जमाने में `XMLHttpRequest` का इस्तेमाल होता था, पर `fetch` अधिक आसान और वादा (promise) आधारित है, जो मॉडर्न ऐप्स के लिए उपयुक्त है। `axios` जैसे लाइब्रेरीज भी अल्टरनेटिव हैं जो `fetch` के समान करते हैं लेकिन कुछ एडिशनल फीचर्स के साथ। `fetch` का उपयोग जावास्क्रिप्ट प्रोमिसेज को पूरा करता है और आपको आसानी से एसिंक्रोनस कोड लिखने में मदद करता है। यह JSON डेटा को हैंडल करने में भी एकदम सुविधाजनक है।

## और जानकारी के लिए (See Also):
- MDN Web Docs `fetch`: [https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)
- `axios` GitHub पेज: [https://github.com/axios/axios](https://github.com/axios/axios)
- जावास्क्रिप्ट प्रोमिसेज समझना: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises)
