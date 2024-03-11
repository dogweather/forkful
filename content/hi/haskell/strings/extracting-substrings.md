---
date: 2024-01-20 17:46:26.950476-07:00
description: "\u0924\u093E\u0930 (strings) \u0915\u0947 \u0935\u093F\u0936\u093F\u0937\
  \u094D\u091F \u0939\u093F\u0938\u094D\u0938\u094B\u0902 \u0915\u094B \u0928\u093F\
  \u0915\u093E\u0932\u0928\u093E \u0924\u092C \u0939\u094B\u0924\u093E \u0939\u0948\
  \ \u091C\u092C \u0939\u092E \u0932\u0902\u092C\u0947 \u091F\u0947\u0915\u094D\u0938\
  \u094D\u091F \u0938\u0947 \u0915\u0941\u091B \u0916\u093E\u0938 \u0909\u092A\u092F\
  \u094B\u0917\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u093F\u092F\u093E\u0902\
  \ \u0932\u0947 \u0930\u0939\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092F\u0939 \u0915\u093E\u092E \u0921\u0947\u091F\u093E \u092A\u0930\u094D\u0938\
  \u093F\u0902\u0917, \u0916\u094B\u091C \u0914\u0930 \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-11T00:14:26.298931-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930 (strings) \u0915\u0947 \u0935\u093F\u0936\u093F\u0937\
  \u094D\u091F \u0939\u093F\u0938\u094D\u0938\u094B\u0902 \u0915\u094B \u0928\u093F\
  \u0915\u093E\u0932\u0928\u093E \u0924\u092C \u0939\u094B\u0924\u093E \u0939\u0948\
  \ \u091C\u092C \u0939\u092E \u0932\u0902\u092C\u0947 \u091F\u0947\u0915\u094D\u0938\
  \u094D\u091F \u0938\u0947 \u0915\u0941\u091B \u0916\u093E\u0938 \u0909\u092A\u092F\
  \u094B\u0917\u0940 \u091C\u093E\u0928\u0915\u093E\u0930\u093F\u092F\u093E\u0902\
  \ \u0932\u0947 \u0930\u0939\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u092F\u0939 \u0915\u093E\u092E \u0921\u0947\u091F\u093E \u092A\u0930\u094D\u0938\
  \u093F\u0902\u0917, \u0916\u094B\u091C \u0914\u0930 \u0921\u0947\u091F\u093E\u2026"
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
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
