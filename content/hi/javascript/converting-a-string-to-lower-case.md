---
title:                "स्ट्रिंग को छोटे अक्षर में बदलना"
html_title:           "Javascript: स्ट्रिंग को छोटे अक्षर में बदलना"
simple_title:         "स्ट्रिंग को छोटे अक्षर में बदलना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ज़रूरत और क्यों?

स्ट्रिंग को लोअर केस में बदलने का मतलब है कि हम उसमे मौजूद सभी अक्षरों को छोटे अक्षरों में बदलकर स्पेस, पंक्ति सेमीकोलन या कोई अन्य चिह्न को अस्पष्ट कर देते हैं। यह प्रोग्रामर्स द्वारा किया जाता है ताकि स्ट्रिंग्स को सामान्य करके समान बनाया जा सके और उन्हें आसानी से प्रोसेस किया जा सके।

## कैसे करें:

```Javascript
let str = "HELLO WORLD";
let lowerStr = str.toLowerCase();

console.log(lowerStr); // hello world
```

```Javascript
let str = "HeLlO wOrLd";
let lowerStr = str.toLowerCase();

console.log(lowerStr); // hello world
```

## गहरी खोज:

- **ऐतिहासिक संदर्भ:** स्ट्रिंग को लोअर केस में बदलने की प्रक्रिया एक प्राचीन प्रोग्रामिंग तकनीक है। इसे पहले, स्ट्रिंग्स को उच्चरण, स्थान, आदि के अनुसार समूहीकृत किया जाता था। परंतु स्मार्ट मोबाइल और वेब डेवलपमेंट ने इसका इस्तेमाल और अधिक उपयोगी बना दिया है।

- **वैकल्पिक तरीके:** अलग-अलग भाषाओं या लाइब्रेरीज में भी यह फ़ंक्शन उपलब्ध हो सकता है जो स्ट्रिंग को लोअर केस में बदलता है। परंतु इनमें से कुछ तकनीक हार्डकोडेड हो सकती है जो अधिक अक्षरों वाले स्ट्रिंग्स पर काम नहीं करती है।

- **अंतर्निहित विवरण:** स्ट्रिंग को लोअर केस में बदलने के लिए, हमारे पास `toLowerCase()` नाम का एक बिल्ट इन फ़ंक्शन है जो String आब्जेक्ट को मॉडिफाइ करके उसे लोअर केस में बदलता है। यह फ़ंक्शन एक नया स्ट्रिंग रिटर्न करता है जो मूल स्ट्रिंग को बदलता है।

## सन्दर्भ:

- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)