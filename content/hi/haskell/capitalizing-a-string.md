---
title:                "Haskell: स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों
यदि आप किसी स्ट्रिंग में कैपिटल लेटर्स को बदलने की कोशिश कर रहे हैं, तो आप शायद सोच रहे होंगे कि आप को हिन्दी पाठकों को इसके बारे में क्या बताने की आवश्यकता है। हालाँकि, हैस्केल में स्ट्रिंग कैपिटलाइज करने के बहुत से तरीके हैं जो आपको पता होने चाहिए। इस ब्लॉग पोस्ट में, हम आपको बताएंगे कि आप किस तरह हैस्केल में स्ट्रिंग कैपिटलैज कर सकते हैं और एक गहरी जानकारी देंगे कि कैपिटल इंग क्या होता है।

## कैसे करें

हैस्केल में स्ट्रिंग कैपिटलाइज करने के लिए, हम अपनी दिखाई गई स्ट्रिंग को लेकर `toUpper()` फंक्शन का उपयोग कर सकते हैं। यह फंक्शन स्ट्रिंग में प्रत्येक अक्षर को ऊपरी स्तर को लेकर उन्नत करता है।


```Haskell
import Data.Char (toUpper)  -- toUpper फंक्शन को लाने के लिए डेटा . चार से आयात करें
capitalize :: String -> String 
capitalize str = map toUpper str
  
main :: IO ()
main = do
    let str = "hello world"
    putStrLn $ capitalize str
```

आपको उपरोक्त उदाहरण में दिखाए गए कोड को कॉपी करके GHCi में चलाकर आप "HELLO WORLD" इस स्ट्रिंग की उत्पन्न योग्यता देख सकते हैं। इस प्रक्रिया को counter-argument करने के लिए, हम 2 अक्षरों को नीचे करीब भी नहीं उत्पत्ति को देख सकते हैं।

```Haskell
import Data.Char (toUpper)

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter (c:str) = toUpper c : str

main :: IO ()
main = do
    let str = "hello world"
    putStrLn $ capitalizeFirstLetter str
```

## गहराई तक

हमने ऊपर दिए गए कोड के साथ स्ट्रिंग कैपिटलाइज करने के दो तरीके देखे