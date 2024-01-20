---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रैंडम नंबर्स का उत्पन्न करना का अर्थ होता है कि प्रोग्रामिंग में कुछ अप्रत्याशित मूल्यों को उत्पन्न करें। प्रोग्राममर्स इसे तब करते हैं जब उन्हें डाटा सर्वर से अव्यक्त तरीके से लेना होता है, या टेस्टिंग के लिए कुछ अनिर्दिष्ट मूल्यों की आवश्यकता होती है।

## कैसे करें :

Elm प्रोग्रामिंग में, रैंडम संख्याएं बनाने के लिए `Random` मॉड्यूल का उपयोग करना होगा। उदाहरण के लिए :

```Elm
import Random

genRandom : Random.Generator Int
genRandom = 
    Random.int 1 100
```
यहां `Random.int 1 100` एक नया रैंडम जनरेटर बनाता है जो 1 और 100 के बीच की संख्याओं को उत्पन्न कर सकता है। 

## गहराई में: 

ऐतिहासिक प्रसंग में, रैंडम संख्या जनरेटिंग कंप्यूटर की शुरुआत से ही रही है। अल्टर्नेटिव्स में आप Pseudo-Random Number Generators (PRNGs) का उपयोग कर सकते हैं जो असलियत में पूरी तरह से यादृच्छिक नहीं होते, लेकिन अधिकांश उपयोगों के लिए पर्याप्त होते हैं। Elm में, `Random` मॉड्यूल PRNGs का उपयोग करता है।

## देखें भी :

1. [Elm का डॉक्यूमेंटेशन Random पैकेज के लिए](http://package.elm-lang.org/packages/elm-lang/core/latest/Random)
2. [रैंडम संख्याओं के उत्पन्न करने के बारे में और अधिक जानकारी](https://en.wikipedia.org/wiki/Random_number_generation)
3. [Pseudo-Random Number Generators के बारे में और अधिक जानकारी](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)