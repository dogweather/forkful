---
date: 2024-01-20 17:46:26.950476-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:54.377456-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
