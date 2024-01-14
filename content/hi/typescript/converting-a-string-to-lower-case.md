---
title:    "TypeScript: स्ट्रिंग को निचले अक्षर में रूपांतरित करना"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# क्यों
कोई भी व्यक्ति एक स्ट्रिंग को लोअर केस में बदलने में रुचि रखता होगा क्योंकि इससे उनकी कोडिंग प्रोसेस को सरल बनाने में मदद मिलती है।

## कैसे करें
टाइपस्क्रिप्ट में एक स्ट्रिंग को लोअर केस में बदलने के लिए निम्नलिखित कोड का उपयोग कर सकते हैं:

```TypeScript
let string = "HELLO";
let lowerCaseString = string.toLowerCase();

console.log(lowerCaseString); // output: hello
```

इस उदाहरण में, हमने `toLowerCase()` फ़ंक्शन का उपयोग करके स्ट्रिंग को लोअर केस में बदल दिया है।

## गहराई में डूबना
स्ट्रिंग को लोअर केस में बदलने के पीछे कारण यह है कि यह उपयोगकर्ताओं को पूरे स्ट्रिंग को छोटे अक्षरों में प्रदर्शित करने से बचाता है। यदि हमें केस सेंसिटिव स्ट्रिंग के साथ काम करना हो, तो हमें स्ट्रिंग को केस सेंसिटिव बनाने के लिए स्ट्रिंग को स्ट्रिंग को पहले लोअर केस बनाने के लिए लोअर केस में बदलने की जरूरत होती है।

## देखें भी
- टाइपस्क्रिप्ट स्ट्रिंग: https://www.typescriptlang.org/docs/handbook/basic-types.html#string
- `toLowerCase()` फ़ंक्शन: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- कास्टिंग टाइपस्क्रिप्ट स्ट्रिंग: https://www.typescriptlang.org/docs/handbook/basic-types.html#type-assertions