---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:46:26.950476-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तार (strings) के विशिष्ट हिस्सों को निकालना तब होता है जब हम लंबे टेक्स्ट से कुछ खास उपयोगी जानकारियां ले रहे होते हैं। यह काम डेटा पर्सिंग, खोज और डेटा प्रोसेसिंग में खासतौर पर उपयोगी होता है।

## How to: (कैसे करें:)
```haskell
-- `take` फ़ंक्शन का इस्तेमाल करके
takeSubstring :: Int -> String -> String
takeSubstring n = take n

-- `drop` और `take` का इस्तेमाल कर विशिष्ट जगह से लेना
extractSubstring :: Int -> Int -> String -> String
extractSubstring start end = take (end - start) . drop start

-- उदाहरण एवं आउटपुट
main :: IO ()
main = do
  let text = "Hello, Haskell!"
  putStrLn $ takeSubstring 5 text -- "Hello"
  putStrLn $ extractSubstring 7 14 text -- "Haskell"
```

## Deep Dive (गहन जानकारी)
तार से उप-तार निकालने का विचार C भाषा के 'substring' कार्यों से विकसित हुआ है। Haskell में, तार को 'lists' के रूप में समझा जाता है, जिससे उप-तार का निर्माण सूची संचालनों के जरिए होता है। `take` और `drop` दो मौलिक फंक्शन हैं जो उपयोगी होते हैं। `Data.Text` मॉड्यूल अन्य विकल्प प्रदान करता है जिसमें `takeEnd`, `dropEnd`, और `splitAt` शामिल हैं जो अधिक कुशलता से काम करते हैं। लेकिन यह सभी ऑपरेशन हास्केल के लेजी इवैल्यूएशन का लाभ उठाते हैं, जिसका अर्थ है कि उप-तार तब तक नहीं बनाया जाता जब तक वह वास्तव में जरूरी न हो।

## See Also (और भी देखें)
- Haskell `Data.Text` पैकेज: [https://hackage.haskell.org/package/text](https://hackage.haskell.org/package/text)
- Lazy Evaluation in Haskell: [https://wiki.haskell.org/Lazy_evaluation](https://wiki.haskell.org/Lazy_evaluation)
- List functions in Haskell: [https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html)
