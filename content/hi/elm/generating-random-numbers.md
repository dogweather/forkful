---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:04.707053-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रैंडम संख्या वह होती है जो किसी निश्चित पैटर्न का अनुसरण नहीं करती। प्रोग्रामर्स इन्हें गेम्स, साइंस सिमुलेशंस, या सिक्योरिटी के लिए सटीकता से प्रयोग करते हैं।

## कैसे करें?

Elm में रैंडम नंबर्स जेनरेट करना `Random` मॉड्यूल का इस्तेमाल करके होता है। ये हमें `Generator` टाइप देता है, जिसके जरिए हम रैंडम वैल्यूज पा सकते हैं।

```
Elm
import Random

randomInt : Int
randomInt =
    Random.step (Random.int 1 100) (Random.initialSeed 42) |> fst
```

यह कोड `1` से `100` के बीच का एक रैंडम इंटीजर बनाता है। याद रखें, Elm में हर बार एक ही `Seed` से शुरू करके हमें समान नंबर मिलेगा।

## गहराई से समझें

रैंडम नंबर जेनरेशन की हिस्ट्री लंबी है - यह पुरानी सिविलाइजेशन से लेकर मॉडर्न कंप्यूटर्स तक पहुँची है। Elm उपयोग करता है प्यूडो-रैंडम नंबर जेनरेटर्स (PRNGs), जो एक डिटर्मिनिस्टिक प्रोसेस है लेकिन आउटपुट रैंडम सा दिखता है। अलग विकल्पों में हम `Random.float`, `Random.list` आदि का उपयोग कर सकते हैं। इम्प्लीमेंटेशन की बात करें तो, Elm एक सीड वैल्यू का इस्तेमाल करता है जिसे शुरुआत में पास किया गया था और उसी को आगे के नंबर्स के जनरेशन में इस्तेमाल करता है।

## संबंधित स्रोत

- [Elm's Random module documentation](http://package.elm-lang.org/packages/elm/random/latest)
- [Intro to Random in Elm by Elm Guide](https://guide.elm-lang.org/effects/random.html)
- [Seed-based randomness in computer programs](https://en.wikipedia.org/wiki/Random_seed)
