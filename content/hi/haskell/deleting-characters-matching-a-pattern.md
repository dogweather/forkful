---
title:                "Haskell: पैटर्न को मिलते हुए अक्षरों को हटाना"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## इसे क्यों करें
कभी-कभी हमें एक पैटर्न से मिलती हुई वेरियेबल्स को हटाने की आवश्यकता होती है जो हमें अपने हास्केल प्रोग्राम में नहीं चाहिए। ऐसे मामलों में, हम अपने कोड से विशेष चरित्रों को हटा सकते हैं।

## कैसे करें
हम एक सरल हास्केल फ़ंक्शन का उपयोग करके स्ट्रिंग से पैटर्न मिलते हुए चरित्रों को हटा सकते हैं।

```Haskell
deleteChars :: String -> String -> String
deleteChars pattern str = filter (\c -> notElem c pattern) str
```

आप इस फ़ंक्शन का उपयोग इस प्रकार कर सकते हैं:

```Haskell
deleteChars "aeiou" "programming" -- OUTPUT: "prgrmmng"
```

## गहरी खुराक
पैटर्न मिलते हुए चरित्रों को हटाने की प्रक्रिया वास्तव में बहुत सरल है। हम स्ट्रिंग पर `filter` फ़ंक्शन का उपयोग करके ऐसा कर सकते हैं। लेकिन अधिक समझने के लिए, हम एक दोस्त फ़ंक्शन का उपयोग कर सकते हैं जो `elem` फ़ंक्शन के ठीक विपरीत है।

```Haskell
notElem :: (Eq a) => a -> [a] -> Bool
notElem _ [] = True
notElem x (y:ys)
  | x == y = False
  | otherwise = notElem x ys
```

यह फ़ंक्शन दी गई स्ट्रिंग से अनुपस्थित चरित्र को हटाता है। अब हमारी `deleteChars` फ़ंक्शन देखते हैं:

```Haskell
deleteChars :: String -> String -> String
deleteChars pattern str = filter (\c -> notElem c pattern) str
```

यहां हमारे फ़ंक्शन को पैरामीटर के रूप में दो स्ट्रिंग पास किए गए हैं। पहला स्ट्रिंग हमारा पैटर्न है जिसे हम अपने चरित्रों से हटाना चाहते हैं। और दूसरा स्ट्रिंग हमारा मूल स्ट्रिंग है जिसमे हम चरित्रों को ढूंढें