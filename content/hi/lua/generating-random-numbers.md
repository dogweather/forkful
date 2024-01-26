---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:50:31.233844-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
यादृच्छिक संख्या उत्पन्न करना यानी अनुमान से बाहर संख्याओं को तैयार करना। प्रोग्रामर ऐसा गेमिंग, सिमुलेशन, और सुरक्षा में परीक्षण के लिए करते हैं।

## How to: (कैसे करें:)
```Lua
-- Lua में यादृच्छिक संख्याएँ बनाना

-- math.randomseed का उपयोग करके बीज निर्धारित करें
math.randomseed(os.time())

-- 0 और 1 के बीच यादृच्छिक संख्या प्राप्त करें
local randomFloat = math.random()
print(randomFloat)

-- 1 से 10 के बीच यादृच्छिक संपूर्ण संख्या प्राप्त करें
local randomInt = math.random(1, 10)
print(randomInt)
```
सैंपल आउटपुट:
```
0.0012512588885158
7
```

## Deep Dive (गहन जानकारी):
Lua में यादृच्छिक संख्या उत्पन्न करना `math.random` फंक्शन से होता है। ये C भाषा के पुस्तकालय `stdlib` पर आधारित है। बीज (`seed`) का उपयोग करना जरूरी है। `os.time()` एक अच्छा बीज देता है क्योंकि हर सेकंड में बदलता रहता है। लेकिन सच्चे यादृच्छिकता के लिए, हार्डवेअर रैण्डम-नंबर जेनरेटर (TRNG) या `/dev/random` जैसे सिस्टम से बीज लेना बेहतर होता है।

वैकल्पिक तरीकों में `math.randomseed`  को एक उन्नत बीज के साथ शुरू करने के लिए उपयोग करने वाले अधिक जटिल एल्गोरिदम शामिल हैं, जैसे कि यूजर के इनपुट या सिस्टम इवेंट्स से बीज उत्पन्न करना। इसकी वजह से परिणाम अधिक यादृच्छिक और अप्रत्याशित होते हैं।

## See Also (यहाँ भी देखें):
- [Lua 5.4 Reference Manual: `math.random`](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Wikipedia: Random Number Generation](https://en.wikipedia.org/wiki/Random_number_generation)
- [Stack Overflow: How to generate a random number in Lua](https://stackoverflow.com/questions/201549/generating-random-integers-in-lua)
