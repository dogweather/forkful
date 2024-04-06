---
date: 2024-01-20 17:46:26.950476-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093E\
  \u0930 \u0938\u0947 \u0909\u092A-\u0924\u093E\u0930 \u0928\u093F\u0915\u093E\u0932\
  \u0928\u0947 \u0915\u093E \u0935\u093F\u091A\u093E\u0930 C \u092D\u093E\u0937\u093E\
  \ \u0915\u0947 'substring' \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0938\u0947\
  \ \u0935\u093F\u0915\u0938\u093F\u0924 \u0939\u0941\u0906 \u0939\u0948\u0964 Haskell\
  \ \u092E\u0947\u0902, \u0924\u093E\u0930 \u0915\u094B 'lists' \u0915\u0947 \u0930\
  \u0942\u092A \u092E\u0947\u0902 \u0938\u092E\u091D\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948,\u2026"
lastmod: '2024-04-05T22:51:07.072719-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0924\u093E\u0930\
  \ \u0938\u0947 \u0909\u092A-\u0924\u093E\u0930 \u0928\u093F\u0915\u093E\u0932\u0928\
  \u0947 \u0915\u093E \u0935\u093F\u091A\u093E\u0930 C \u092D\u093E\u0937\u093E \u0915\
  \u0947 'substring' \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0938\u0947 \u0935\
  \u093F\u0915\u0938\u093F\u0924 \u0939\u0941\u0906 \u0939\u0948\u0964 Haskell \u092E\
  \u0947\u0902, \u0924\u093E\u0930 \u0915\u094B 'lists' \u0915\u0947 \u0930\u0942\u092A\
  \ \u092E\u0947\u0902 \u0938\u092E\u091D\u093E \u091C\u093E\u0924\u093E \u0939\u0948\
  , \u091C\u093F\u0938\u0938\u0947 \u0909\u092A-\u0924\u093E\u0930 \u0915\u093E \u0928\
  \u093F\u0930\u094D\u092E\u093E\u0923 \u0938\u0942\u091A\u0940 \u0938\u0902\u091A\
  \u093E\u0932\u0928\u094B\u0902 \u0915\u0947 \u091C\u0930\u093F\u090F \u0939\u094B\
  \u0924\u093E \u0939\u0948\u0964 `take` \u0914\u0930 `drop` \u0926\u094B \u092E\u094C\
  \u0932\u093F\u0915 \u092B\u0902\u0915\u094D\u0936\u0928 \u0939\u0948\u0902 \u091C\
  \u094B \u0909\u092A\u092F\u094B\u0917\u0940 \u0939\u094B\u0924\u0947 \u0939\u0948\
  \u0902\u0964 `Data.Text` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0905\u0928\
  \u094D\u092F \u0935\u093F\u0915\u0932\u094D\u092A \u092A\u094D\u0930\u0926\u093E\
  \u0928 \u0915\u0930\u0924\u093E \u0939\u0948 \u091C\u093F\u0938\u092E\u0947\u0902\
  \ `takeEnd`, `dropEnd`, \u0914\u0930 `splitAt` \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948\u0902 \u091C\u094B \u0905\u0927\u093F\u0915 \u0915\u0941\u0936\u0932\u0924\
  \u093E \u0938\u0947 \u0915\u093E\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\
  \u0964 \u0932\u0947\u0915\u093F\u0928 \u092F\u0939 \u0938\u092D\u0940 \u0911\u092A\
  \u0930\u0947\u0936\u0928 \u0939\u093E\u0938\u094D\u0915\u0947\u0932 \u0915\u0947\
  \ \u0932\u0947\u091C\u0940 \u0907\u0935\u0948\u0932\u094D\u092F\u0942\u090F\u0936\
  \u0928 \u0915\u093E \u0932\u093E\u092D \u0909\u0920\u093E\u0924\u0947 \u0939\u0948\
  \u0902, \u091C\u093F\u0938\u0915\u093E \u0905\u0930\u094D\u0925 \u0939\u0948 \u0915\
  \u093F \u0909\u092A-\u0924\u093E\u0930 \u0924\u092C \u0924\u0915 \u0928\u0939\u0940\
  \u0902 \u092C\u0928\u093E\u092F\u093E \u091C\u093E\u0924\u093E \u091C\u092C \u0924\
  \u0915 \u0935\u0939 \u0935\u093E\u0938\u094D\u0924\u0935 \u092E\u0947\u0902 \u091C\
  \u0930\u0942\u0930\u0940 \u0928 \u0939\u094B\u0964."
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
