---
date: 2024-01-20 18:01:11.598799-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) TypeScript\
  \ \u092E\u0947\u0902 HTTP request \u092D\u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `fetch` API \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0947\u0902\u0964 \u0928\u0940\u091A\u0947 \u0907\u0938\u0915\u093E \u090F\u0915\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:51.883718-06:00'
model: gpt-4-1106-preview
summary: "TypeScript \u092E\u0947\u0902 HTTP request \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `fetch` API \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0947\u0902\u0964 \u0928\u0940\u091A\u0947 \u0907\u0938\u0915\
  \u093E \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
TypeScript में HTTP request भेजने के लिए `fetch` API का उपयोग करें। नीचे इसका एक उदाहरण है:

```typescript
async function fetchData(url: string): Promise<void> {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    const data = await response.json();
    console.log(data);
  } catch (error) {
    console.error('Fetch error:', error.message);
  }
}

fetchData('https://jsonplaceholder.typicode.com/todos/1');
```

जब आप ऊपर के कोड को चलाएंगे, तो आपको कंसोल में JSON डेटा नजर आएगा:

```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive (गहराई में जानकारी:)
HTTP requests का इतिहास 1990s से शुरू होता है, जब World Wide Web नया था। AJAX (Asynchronous JavaScript and XML) तकनीक ने दिखाया कि requests को पेज को रिफ्रेश किए बिना भी भेजा जा सकता है। TypeScript `fetch` API इसी का एक आधुनिक रूप है।

Fetch API के अलावा, `XMLHttpRequest` भी एक विकल्प है, पर ये पुराना और कम सुविधाजनक है। `Axios` जैसे तृतीय-पक्ष लाइब्रेरीज भी हैं जो ज़्यादा सुविधाएँ और ब्राउज़र समर्थन देते हैं।

जब आप `fetch` का उपयोग करते हैं, Promise पर आधारित `async` और `await` शेली कोड को ज्यादा पढ़ने योग्य और संभालने में आसान बनाती है।

## See Also (और देखें:)
- Mozilla MDN Web Docs पर Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- हाई परफॉर्मेंस ब्राउज़र नेटवर्किंग (HTTP/2, Fetch, etc.): https://hpbn.co/
- Axios GitHub repository: https://github.com/axios/axios
