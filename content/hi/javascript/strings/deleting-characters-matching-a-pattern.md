---
date: 2024-01-20 17:43:10.251360-07:00
description: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932\
  \ \u0916\u093E\u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\u0948\u0930\u0947\u0915\
  \u094D\u091F\u0930\u094D\u0938 \u0915\u094B \u0921\u093F\u0932\u0940\u091F \u0915\
  \u0930\u0928\u093E \u0907\u0938\u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948\
  \ \u0915\u093F \u0939\u092E \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u092E\u0947\u0902 \u0938\u0947 \u0935\u093F\u0936\u0947\u0937 \u0905\u0915\
  \u094D\u0937\u0930 \u092F\u093E \u0936\u092C\u094D\u0926\u094B\u0902 \u0915\u094B\
  \ \u0939\u091F\u093E \u0926\u0947\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u093E\u095E\u2026"
lastmod: '2024-03-13T22:44:52.963645-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0928\u0947 \u0935\u093E\u0932\u0947 \u0915\u0948\u0930\u0947\u0915\u094D\
  \u091F\u0930\u094D\u0938 \u0915\u094B \u0921\u093F\u0932\u0940\u091F \u0915\u0930\
  \u0928\u093E \u0907\u0938\u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\
  \u093F \u0939\u092E \u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u0938\u0947 \u0935\u093F\u0936\u0947\u0937 \u0905\u0915\u094D\
  \u0937\u0930 \u092F\u093E \u0936\u092C\u094D\u0926\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E \u0926\u0947\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\
  \u0947\u091F\u093E \u0915\u094B \u0938\u093E\u095E\u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न से मेल खाने वाले कैरेक्टर्स को डिलीट करना इसका मतलब है कि हम एक स्ट्रिंग में से विशेष अक्षर या शब्दों को हटा देते हैं। प्रोग्रामर्स इसे डेटा को साफ़ करने, उपयोग के लिए तैयार करने, या अनचाहे टेक्स्ट को हटाने के लिए करते हैं।

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
