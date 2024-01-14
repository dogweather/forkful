---
title:                "Haskell: स्ट्रिंग में मज़ेदार प्रपत्र (Capitalizing a string)"
simple_title:         "स्ट्रिंग में मज़ेदार प्रपत्र (Capitalizing a string)"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Haskell में अपनी स्ट्रिंग को कैपिटलाइज करने का क्या फायदा हो सकता है? 

## क्यों

एक स्ट्रिंग को कैपिटलाइज करने से प्रत्येक शब्द केवल अपने पहले अक्षर को बड़ा करके या उपर नीचे के साथ एक स्पेशल कैरेक्टर को बिना बॉझे समाप्त कर सकते हैं। इससे आपके कोड में अधिक उचितता बनेगी और आसानी से समझा जा सकेगा।

## कैसे करे

नीचे दिए गए है Haskell में स्ट्रिंग को कैपिटलाइज करने का एक सरल तरीका।

```Haskell
capitialize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
```

इसके बाद, आप इस फ़ंक्शन को अपनी स्ट्रिंग पर लागू कर सकते हैं:

```Haskell
capitalize "haskell" -- Output: "Haskell"
```

यदि आप अपनी स्ट्रिंग को सभी कैपिटल लेटर्स में बदलना चाहते हैं तब आप `map toUpper` क्रिया का उपयोग कर सकते हैं:

```Haskell
map toUpper "haskell" -- Output: "HASKELL"
```

## गहराईगत जाँच

हमारे `capitalize` फ़ंक्शन में, हमने `x` को अपने पहले अक्षर को अपरकेस में बदलने के लिए `toUpper` फ़ंक्शन का उपयोग किया है। आप हमेशा से `toLower` फ़ंक्शन का भी उपयोग कर सकते हैं यदि आप स्ट्रिंग को नीचे के साथ कैपिटलाइज करना चाहते हैं।

अतिरिक्त रूप से, यदि आप निर्दिष्ट स्ट्रिंग में सिर्फ अक्षरों को ही कैपिटलाइज करना चाहते हैं, तो आप `isAlpha` फ़ंक्शन और `toUpper` फ़ंक्शन का उपयोग कर सकते हैं। निम्न उदाहरण में, हमने सिर्फ वर्णमाला वर्णों को कैपिटलाइज क