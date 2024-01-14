---
title:    "Haskell: रैंडम नंबर उत्पन्न करना"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों:

क्या आप कभी हेस्केल में रैंडम नंबर्स बनाने के बारे में सोचा है? और क्या आप इन रैंडम नंबर्स का उपयोग अपने कोड में करना चाहते हैं? अगर हां, तो आप सही जगह पर हैं। हम आपको बताएंगे कि हेस्केल में रैंडम नंबर्स कैसे बनाएं और इनका उपयोग कैसे करें।

## कैसे करें:

हेस्केल में रैंडम नंबर्स बनाने के लिए आप `Random` में इनबिल्ट फंक्शन `randomRIO` का उपयोग कर सकते हैं। इस फंक्शन का उपयोग करके आप किसी भी रेंज में से एक रैंडम नंबर बना सकते हैं। यहां एक उदाहरण है:

```Haskell
import System.Random

main = do
  randomNum <- randomRIO (1, 10) -- 1 से 10 तक का रैंडम नंबर
  putStrLn $ "रैंडम नंबर: " ++ show randomNum
```

आउटपुट:
```
रैंडम नंबर: 7
```

## डीप डाइव:

हेस्केल में इनबिल्ट फंक्शन `randomRIO` के अलावा भी कई और तरीके हैं रैंडम नंबर्स बनाने के। आप `System.Random` मॉन्डल का उपयोग करके `mkStdGen` और `random` फंक्शन का उपयोग कर सकते हैं। इसके अलावा आप अपने अनुभव के अनुसार अपने रैंडम नंबर्स बनाने के लिए अन्य प्रोग्रामिंग टेक्निक्स भी अपना सकते हैं।

## अधिक जानकारी के लिए देखें:

* [Haskell में रैंडम नंबर्स बनाना](https://wiki.haskell.org/Random)
* [Haskell मॉन्डल: System.Random](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html)