---
title:    "Elm: टेक्स्ट खोज और प्रतिस्थापन करना"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी भाषा बदलते हुए पाठ को खोजने और प्रतिस्थापित करने में देरी हुई है? Elm में यह काम करना बेहद आसान है और उसे समझने से आपके कोड को धीरे-धीरे अधिक अनुकूलित करने में मदद मिलेगी।  

## कैसे

आपको सरल उदाहरण और अभ्यास के लिए "```` Elm```" कोड ब्लॉक के भीतर प्रदर्शित कोड को अपनाने की आवश्यकता हो सकती है। यह आपको बताएगा कि आप कैसे एक पाठ का प्रतिस्थापन कर सकते हैं।

````Elm
-- यहां हम पाठ के साथ व्याख्या कर सकते हैं
-- वर्तमान कोड में "Hello" को "नमस्ते" के साथ प्रतिस्थापित करता है
String.replace "Hello" "नमस्ते" यो यह देखें : "नमस्ते दुनिया"
````

## गहरी खोज

पाठ को परिवर्तित करने के अतिरिक्त, Elm में आप लंबे पाठों और विशेषताओं को खोजने के लिए विस्तृत तकनीकों का उपयोग कर सकते हैं। "String.replace" एक बहुत ही साधारण और स्पष्ट तकनीक है जिसमें आप केवल दो वास्तविक पाठों को प्रदान करते हैं। यदि आपको अधिक नियंत्रण चाहिए, तो आप "Regex.replace" का उपयोग कर सकते हैं, जो सामान्य अभिव्यक्तियों के साथ काम कर सकता है।

## देखें भी

- [Elm Docs - String module] (https://package.elm-lang.org/packages/elm/core/latest/String) 
- [Elm Docs - Regex module] (https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Elm Guide - Text and Strings] (https://guide.elm-lang.org/appendix/text.html)