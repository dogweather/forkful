---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

"string को lower case में बदलना" - इसका मतलब है की एक वाक्यांश (जिसे हम स्ट्रिंग कहते हैं) को हम कम टाइप करने के लिए बदल देते हैं। प्रोग्रामर इसका उपयोग उस समय करते हैं जब वे चाहते हैं कि उनका कोड केस-संवेदनशील न हो - अर्थात, कस्टमर्स चाहे upper case में टाइप करें या lower case, कोड सही धंग से काम करता है।

## कैसे:

TypeScript का `toLowerCase()` फ़ंक्शन उपयोग करें। यहां एक उदाहरण है:

```typescript
let greet: string = "नमस्ते, DUNIYA!";
console.log(greet.toLowerCase());
```
और इसका आउटपुट होगा:
```
नमस्ते, duniya!
```

## गहराई में:

1. ऐतिहासिक प्रसंग: यह विधि JavaScript (जिसके ऊपर TypeScript निर्माणित है) से सीधे उद्धृत है और उसका उपयोग केस-अनिर्भिन्न कोड को सशक्त बनाने में किया जाता है।
   
2. विकल्प: एक विकल्प `toLocaleLowerCase()` है जो विभिन्न भाषाओं और लोकेल पर आधारित होता है।
   
3. आंतर क्रियान्वित विवरण: `toLowerCase()` फ़ंक्शन प्रत्येक केस को संगणक की भाषा में पूर्वनिर्धारित "निचले केस" संपर्कों के अनुसार निर्धारित करता है।

## देखें भी:

1. [Mozilla Developer Network - toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
2. [Microsoft TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)