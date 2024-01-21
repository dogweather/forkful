---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T18:00:25.229883-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTTP अनुरोध (request) भेजना इंटरनेट पर सर्वर से डेटा मांगने का तरीका है। प्रोग्रामर इसे वेबसाइट्स का डेटा प्राप्त करने, API से बात करने या फॉर्म डेटा भेजने के लिए करते हैं।

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