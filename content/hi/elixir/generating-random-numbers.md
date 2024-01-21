---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:42.387668-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर जेनरेटिंग यानी बेतरतीब संख्या निर्माण, यह है अप्रत्याशित संख्याएँ उत्पन्न करना। प्रोग्रामर्स इसे गेमिंग, सिमुलेशन्स, और सुरक्षा में उपयोग करते हैं।

## How to (कैसे करें):
Elixir में रैंडम नंबर उत्पन्न करने के लिए `:rand.uniform` मॉड्यूल का इस्तेमाल करें। यहाँ देखिए:

```elixir
# एक रैंडम इंटिजर 1 से 10 के बीच
random_num = :rand.uniform(10)
IO.puts(random_num)
```

सैंपल आउटपुट:
```
7
```

```elixir
# एक फ्लोटिंग पॉइंट नंबर 0 से 1 के बीच
random_float = :rand.uniform()
IO.puts(random_float)
```

सैंपल आउटपुट:
```
0.443586
```

## Deep Dive (विस्तार से जानकारी):
Elixir का रैंडम नंबर जेनरेटर एर्लांग का `:rand` मॉड्यूल इस्तेमाल करता है। इसका मतलब है कि Elixir का पर्दे के पिछे काम करने का तरीका Erlang पर आधारित है। पहले `:random` मॉड्यूल था, पर उसे `:rand` से रिप्लेस किया गया क्योंकि यह बेहतर और अधिक विश्वसनीय प्रदान करता है। 

अलग-अलग प्रकारों के रैंडम नंबर जेनरेटर्स हैं, जैसे कि लीनियर कांग्रुएंटियल जेनरेटर्स, मार्सेन ट्विस्टर, और क्रिप्टोग्राफिक जेनरेटर्स। `:rand` मॉड्यूल छिपे हुए स्टेट्स रखता है और शुरुआत में एक सीड की आवश्यकता होती है, जिसे ऑटोमैटिकली सेट किया जा सकता है या मैन्युअली `:rand.seed/1` या `:rand.seed/3` का उपयोग करके।

क्रिप्टोग्राफिकली सेफ रैंडम नंबर्स की जरुरत हो, तो Elixir में हम `:crypto.strong_rand_bytes/1` का उपयोग कर सकते हैं।

## See Also (और भी जानकारी):
- Erlang `:rand` मॉड्यूल डॉक्स: [https://erlang.org/doc/man/rand.html](https://erlang.org/doc/man/rand.html)
- Elixir ऑफिशियल डॉक्स: [https://hexdocs.pm/elixir/Kernel.html](https://hexdocs.pm/elixir/Kernel.html)
- क्रिप्टोग्राफी और रैंडम नंबर्स: [https://learnyousomeerlang.com/secure-erlang](https://learnyousomeerlang.com/secure-erlang)