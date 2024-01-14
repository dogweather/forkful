---
title:                "TypeScript: स्ट्रिंग को निचे आकार में बदलना"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी व्यक्ति एक स्ट्रिंग को लोअर केस में रूपांतरित करने में रुचि रख सकता है, जो उन्हें कॉडिंग काम को आसान और उत्पादक बनाता है। यह एक बहुत आसान और उपयोगी काम होता है जो लगभग हर प्रोग्रामिंग भाषा में उपलब्ध होता है।

## कैसे करें

अगर आपको अपनी स्ट्रिंग को लोअर केस में रूपांतरित करना है, तो आप निम्नलिखित टाइपस्क्रिप्ट कोड का उपयोग कर सकते हैं।

```TypeScript
let str: string = "HELLO";
console.log(str.toLowerCase());
```

आपको निम्नलिखित आउटपुट मिलेगा:

```TypeScript
hello
```


## डीप डाइव

स्ट्रिंग को लोअर केस में रूपांतरित करने के लिए टाइपस्क्रिप्ट में बहुत से युटिलिटी फंक्शन्स उपलब्ध हैं। उनमें से कुछ प्रमुख फंक्शन्स हैं `toLowerCase()` और `toLocaleLowerCase()`। ये दोनों फंक्शन्स स्ट्रिंग को लोअर केस में रूपांतरित करते हैं और दोनों में कोई अंतर नहीं है।

उनकी जगह आप यदि चाहें, `toUpperCase()` या `toLocaleUpperCase()` भी उपयोग कर सकते हैं, जो स्ट्रिंग को अपर केस में रूपांतरित करेंगे।

## देखें भी

- [TypeScript सह कोडिंग का उपयोग क्यों और कैसे करें?](https://www.example.com/typescript-coding-guide)
- [टाइपस्क्रिप्ट में स्ट्रिंग Manipulation की समस्याओं का समाधान कैसे करें?](https://www.example.com/string-manipulation-typescript)