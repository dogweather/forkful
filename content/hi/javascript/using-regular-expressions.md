---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन (Regular Expressions) यानी RegExp, एक पैटर्न है जिसका उपयोग स्ट्रिंग से डेटा खोजने के लिए किया जाता है। प्रोग्रामर्स RegExp का उपयोग इसलिए करते हैं क्योंकि यह टेक्स्ट प्रोसेसिंग को आसान और फास्ट बनाता है।

## How to: (कैसे करें:)
```javascript
// सरल RegExp - मोबाइल नंबर्स का पैटर्न खोजना
let text = 'मेरा मोबाइल नंबर 9876543210 है।';
let regex = /\b\d{10}\b/;
let found = text.match(regex);
console.log(found[0]); // 9876543210

// ग्लोबल सर्च - सभी ईमेल पते ढूँढना
let emailText = 'संपर्क करें: example@test.com, info@test.co.in';
let emailRegex = /\S+@\S+\.\S+/g;
let emailFound = emailText.match(emailRegex);
console.log(emailFound); // ['example@test.com', 'info@test.co.in']
```

## Deep Dive (गहराई से जानकारी):
1. **ऐतिहासिक संदर्भ**: RegExp का आविष्कार 1950-60 के दशक में हुआ था और यह टेक्स्ट सर्च और मैनिपुलेशन की दुनिया में क्रांतिकारी साबित हुआ।
2. **विकल्प**: RegExp के अलावा, सामान्य स्ट्रिंग मेथड्स जैसे `indexOf()`, `lastIndexOf()`, `startsWith()`, `endsWith()` आदि का उपयोग करके भी सर्चिंग और मैचिंग की जा सकती है, लेकिन ये RegExp की शक्ति और लचीलापन प्रदान नहीं करते।
3. **कार्यान्वयन विवरण**: JavaScript में RegExp ऑब्जेक्ट का निर्माण दो तरीकों से किया जा सकता है - लिटरल नोटेशन (`/abc/`) या कंस्ट्रक्टर (`new RegExp('abc')`) का उपयोग करके। `g` फ्लैग ग्लोबल सर्च के लिए, `i` फ्लैग केस इनसेंसिटिव मैच के लिए और `m` फ्लैग मल्टी-लाइन सर्च के लिए होता है। 

## See Also (इसे भी देखें):
- MDN Web Docs पर RegExp गाइड: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- RegExp टेस्टिंग टूल: [RegExr](https://regexr.com/)
- RegExp ट्यूटोरियल: [Regular Expressions Tutorial](https://www.regular-expressions.info/javascript.html)
