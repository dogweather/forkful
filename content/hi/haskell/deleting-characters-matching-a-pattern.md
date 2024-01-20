---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Haskell में पैटर्न से मेल खाते वर्णों को हटाना: कैसे और क्यों?

## क्या और क्यों?     
कभी-कभी प्रोग्रामर्स को पैटर्न से मिलने वाले वर्णों को हटाने की आवश्यकता होती है। यह तब होता है जब उन्हें एक विशेष शब्द को एक स्ट्रिंग से निष्कासित करने की आवश्यकता होती है। 

## कैसे करें: 
इसे हम इस प्रकार कर सकते हैं:

```Haskell
import Data.Char (isSpace)
import Data.List (dropWhile)

trim :: String -> String
trim = dropWhile isSpace
```
जब आप कोड को चलायेंगे, यह स्पेस के साथ शुरू होने वाले वर्ण को हटा देगा।

## गहरी जानकारी:
1. ऐतिहासिक संदर्भ: Haskell, 1990 में रिलीज हुआ था और इसे लेंडिन यूनिवर्सिटी कोलेज द्वारा विकसित किया गया था।
2. विकल्प: आप अन्य भाषाओं, जैसे कि Python और Java, का भी उपयोग कर सकते हैं, लेकिन Haskell फ़ंक्शनल प्रोग्रामिंग भाषा होने के कारण अधिक उपयोगी हो सकता है।  
3. क्रियान्वयन विवरण: इस कोड का उपयोग करके, आंतरिक रूप से, हम पहले यह पहचानते हैं कि क्या स्वरूप है और फिर उसे हटा देते हैं।  

## आगे पढ़ें:
1. [Haskell Documentation](https://www.haskell.org/documentation/)
2. [Haskell Functions](https://www.haskell.org/tutorial/functions.html)
3. [Functional Programming in Haskell](https://www.futurelearn.com/courses/functional-programming-haskell)