---
title:    "TypeScript: स्ट्रिंग को निचले अक्षर में बदलना"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

स्ट्रिंग को लोअर केस में कनवर्ट करने के फायदे हैं कि यह इसे विभिन्न प्रोग्रामिंग कार्यों में उपयोग करने के लिए बनाता है, जैसे कि स्ट्रिंग में खोज करना या स्ट्रिंग को विशेष पात्रों के साथ तुलना करना।

## कैसे करें

```TypeScript
let str: string = "Hello World";
console.log(str.toLowerCase());
```

आउटपुट:
```
hello world
```

यहां हमने `toLowerCase()` एक बिल्ट-इन मेथड का उपयोग करके स्ट्रिंग को लोअर केस में कनवर्ट किया। हम `str` स्ट्रिंग के लिए `toLowerCase()` के सिर्फ एक आगे प्रोटोटाइप मेथड को बनाएंगे और उसे इससे कॉल करेंगे। लेकिन `toLowerCase()` का असली का काम यह है कि यह किसी भी स्ट्रिंग को लोअर केस में कनवर्ट करेगा।

## गहराई में जाएं

जब हम एक स्ट्रिंग को `toLowerCase()` के साथ कॉल करते हैं, तो यह स्ट्रिंग को लोअर केस में कनवर्ट कर देता है। इस कारण से, हम अलग-अलग प्रकार के मामलों को लोअर केस में देख सकते हैं। इसकी एक उदाहरण के लिए, हम विशेष पात्रों की तुलना कर सकते हैं, जैसे कि:

```TypeScript
let str1: string = "hello";
let str2: string = "Hello";

console.log(str1.toLowerCase() === str2.toLowerCase()); // true
```

यहां हमने स्ट्रिंग `str1` को `toLowerCase()` के साथ कॉल किया और उसे `str2` के साथ तुलना किया। दोनों स्ट्रिंग लोअर केस में हैं, इसलिए यह समान माने जाएंगे। इसी तरह, हम खोज भी सकते हैं और स्ट्रिंग को उसके समान खोज से मेल खाते हैं।

## देखें भी

- [String.prototype.toLowerCase() जावास्क्रिप्ट डाक रसीद।](https://developer.mozilla