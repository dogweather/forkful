---
date: 2024-01-20 17:43:10.251360-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0907\u0938\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u092E\u0947\u0902, \u0939\u092E\u0928\u0947\
  \ `.replace()` \u092E\u0947\u0925\u0921 \u0914\u0930 \u0930\u0947\u0917\u0941\u0932\
  \u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\u0947\u0936\u0928 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u091F\
  \u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0947 \u0935\u093F\u0936\u0947\u0937\
  \ \u091A\u093F\u0928\u094D\u0939\u094B\u0902 \u0915\u094B \u0939\u091F\u093E \u0926\
  \u093F\u092F\u093E \u0939\u0948\u0964."
lastmod: '2024-04-05T21:53:54.913759-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0907\u0938 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u092E\u0947\u0902, \u0939\u092E\u0928\u0947 `.replace()`\
  \ \u092E\u0947\u0925\u0921 \u0914\u0930 \u0930\u0947\u0917\u0941\u0932\u0930 \u090F\
  \u0915\u094D\u0938\u092A\u094D\u0930\u0947\u0936\u0928 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u091F\u0947\u0915\
  \u094D\u0938\u094D\u091F \u0938\u0947 \u0935\u093F\u0936\u0947\u0937 \u091A\u093F\
  \u0928\u094D\u0939\u094B\u0902 \u0915\u094B \u0939\u091F\u093E \u0926\u093F\u092F\
  \u093E \u0939\u0948\u0964."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
```Javascript
let text = "नमस्ते! क्या हाल हैं? सब ठीक?";

// वाक्य चिन्ह हटाने के लिए रेगुलर एक्सप्रेशन का इस्तेमाल
let cleanText = text.replace(/[!?.]/g, "");
console.log(cleanText); // Output: "नमस्ते क्या हाल हैं सब ठीक"
```

इस उदाहरण में, हमने `.replace()` मेथड और रेगुलर एक्सप्रेशन का उपयोग करते हुए टेक्स्ट से विशेष चिन्हों को हटा दिया है।

## Deep Dive (गहन जानकारी)
पैटर्न मैचिंग और कैरेक्टर्स को डिलीट करने की प्रक्रिया जावास्क्रिप्ट में रेगुलर एक्सप्रेशन्स के प्रयोग से शुरू होती है जो की 1960s में कंप्यूटर साइंस की दुनिया में आई थी। `String.prototype.replace()` एक शक्तिशाली मेथड है जिसका इस्तेमाल करके डेवलपर्स आसानी से टेक्स्ट को मनचाहा रूप दे सकते हैं।

वैकल्पिक तरीके के रूप में, आप `String.prototype.split()` और `Array.prototype.join()` को भी इस्तेमाल कर सकते हैं, लेकिन ये थोड़ा लंबा प्रोसेस हो सकता है। रेगुलर एक्सप्रेशन के इस्तेमाल से हम जटिल पैटर्न्स को भी आसानी से मैनेज कर सकते हैं।

## See Also (और देखें:)
- MDN Web Docs on `.replace()`: [MDN replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Regular Expressions in JavaScript: [Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- JavaScript String Methods: [String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
