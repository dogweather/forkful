---
title:    "Javascript: प्रामाणिक अभिव्यक्तियों का उपयोग करना"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

जब आप कोड करने के लिए जावास्क्रिप्ट का उपयोग करते हैं, आपके पास तीन विकल्प होते हैं - स्थानिक, ग्लोबल और वस्त्रनिष्ठ. टेक्स्ट को स्थानिक एवं ग्लोबल विकल्पों में तालिकांकित और हाइलाइट करने के लिए आपको नियम (regular expressions) के उपयोग से फायदा हो सकता है।

## कैसे करें

```Javascript
// स्थानिक विकल्प को देखने के लिए
var text = "यह एक वस्त्रनिष्ठ विकल्प है। यह एक विस्तृत पाठ है।";
var pattern1 = /वस्त्रनिष्ठ/; // एक मैच देखें

console.log(pattern1.test(text)); // true
console.log(pattern1.exec(text)); // ["वस्त्रनिष्ठ", index: 8, input: "यह एक वस्त्रनिष्ठ विकल्प है। यह एक विस्तृत पाठ है।"]

// ग्लोबल विकल्प को देखने के लिए
var pattern2 = /एक/g; // सभी मैच ढूंढें

console.log(pattern2.test(text)); // true
console.log(text.match(pattern2)); // ["एक", "एक"]
```

## गहराई संचरण

नियम (regular expressions) एक शक्तिशाली और प्रभावी तरीका है जो टेक्स्ट को ढूंढने और प्रोसेस करने में मदद करता है। आप टेक्स्ट में एक विशिष्ट पैटर्न (pattern) का ढूंढ़ने और प्रत्येक मैच को वस्त्रनिष्ठ बदलाव करने में इसका उपयोग कर सकते हैं। यह सरल स्ट्रिंग प्रोसेसिंग के साथ-साथ आपको दोहरी लाभ प्रदान करता है - धुरी लोकोत्तर सर्च और टेक्स्ट प्रोसेसिंग स्पीड को बढ़ाने में मदद करता है।

## देखें

[जावास्क्रिप्ट रेगुलर एक्सप्रेशन्स का आसान उपयोग](https://www.tutorialspoint.com/javascript/javascript_regular_expressions.htm)
[कामकाजी निय