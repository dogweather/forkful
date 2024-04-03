---
date: 2024-01-26 00:59:33.539499-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: TypeScript \u092E\
  \u0947\u0902, \u090F\u0930\u0930 \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u0905\u0915\u094D\u0938\u0930 `try`, `catch`, \u0914\u0930\
  \ `finally` \u092C\u094D\u0932\u0949\u0915\u094D\u0938 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
lastmod: '2024-03-13T22:44:51.902066-06:00'
model: gpt-4-1106-preview
summary: "TypeScript \u092E\u0947\u0902, \u090F\u0930\u0930 \u0939\u0948\u0902\u0921\
  \u0932\u093F\u0902\u0917 \u092E\u0947\u0902 \u0905\u0915\u094D\u0938\u0930 `try`,\
  \ `catch`, \u0914\u0930 `finally` \u092C\u094D\u0932\u0949\u0915\u094D\u0938 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0939\u0948\u0964\
  ."
title: "\u090F\u0930\u0930\u094D\u0938 \u0915\u094B \u0939\u0948\u0902\u0921\u0932\
  \ \u0915\u0930\u0928\u093E"
weight: 16
---

## कैसे करें:
TypeScript में, एरर हैंडलिंग में अक्सर `try`, `catch`, और `finally` ब्लॉक्स का उपयोग होता है।

```typescript
function riskyOperation() {
  throw new Error("कुछ गलत हो गया!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("एरर पकड़ा गया:", error.message);
  } finally {
    console.log("यह हमेशा चलता है, चाहे एरर हो या नहीं।");
  }
}

handleErrors();
```

नमूना आउटपुट:

```
एरर पकड़ा गया: कुछ गलत हो गया!
यह हमेशा चलता है, चाहे एरर हो या नहीं।
```

प्रॉमिसेज के साथ एसिंक्रोनस उदाहरण:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // एक एरर का अनुकरण करें
    reject("बुरी तरह विफल");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("एसिंक्रोनस एरर पकड़ा गया:", error);
  }
}

handleAsyncErrors();
```

नमूना आउटपुट:

```
एसिंक्रोनस एरर पकड़ा गया: बुरी तरह विफल
```

## गहराई में
प्रोग्रामिंग के आरंभ से ही एरर हैंडलिंग एक मुख्य आधार बन गया है। TypeScript में, जो जावास्क्रिप्ट पर आधारित है, ECMAScript 2017 में async/await के परिचय के साथ एरर हैंडलिंग और अधिक मजबूत हो गई। इससे पहले, एसिंक्रोनस कोड में गलतियों को संभालने के लिए हम अक्सर कॉलबैक फ़ंक्शंस और प्रॉमिसेज पर निर्भर थे।

TypeScript में `try/catch` के एक विकल्प के रूप में React जैसे फ्रेमवर्क द्वारा प्रदान किए गए एरर बाउंड्रीज़ का उपयोग किया जा सकता है। सर्वर-साइड हैंडलिंग के लिए, हम Express.js जैसे प्लेटफार्मों में मिडलवेयर का उपयोग कर सकते हैं ताकि एरर प्रबंधन को केंद्रीकृत कर सकें।

कार्यान्वयन के दृष्टिकोण से, TypeScript का अपना खुद का एरर हैंडलिंग मैकेनिज़म नहीं है लेकिन यह जावास्क्रिप्ट के एरर हैंडलिंग पर निर्भर करता है। कस्टम एरर क्लासेस `Error` क्लास को विस्तार दे सकती हैं ताकि और वर्णनात्मक एरर सूचना प्रदान कर सकें।

## साथ ही देखें
- [MDN पर try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [MDN पर Async/Await](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [React में एरर बाउंडरीज़ का प्रयोग](https://reactjs.org/docs/error-boundaries.html)
- [Express.js एरर हैंडलिंग](https://expressjs.com/en/guide/error-handling.html)
