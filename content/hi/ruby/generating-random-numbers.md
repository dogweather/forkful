---
title:                "रैंडम नंबर उत्पन्न करना"
html_title:           "Ruby: रैंडम नंबर उत्पन्न करना"
simple_title:         "रैंडम नंबर उत्पन्न करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों करते हैं?
"रैंडम नंबर जेनरेट करना" एक तरीका है जिससे हम यादृच्छिक तौर पर संख्याओं का उत्पादन कर सकते हैं। यह प्रोग्रामर्स किसी भी उदाहरण समस्या को हल करने के लिए उपयोग करते हैं, जो संख्या या यादृच्छिक परिपटन से आधारित हो सकती है।

## कैसे करें:
```Ruby
# एक से दस तक की यादृच्छिक संख्याओं की जेनरेट करें
puts rand(1..10)

# दो से दस तक की यादृच्छिक संख्याओं का एक सरणी बनाएं
array_of_numbers = (2..10).to_a.shuffle
puts array_of_numbers

# यादृच्छिक बाईनरी संख्याओं का उत्पादन करें
puts rand.to_s(2)
```

आउटपुट:
```
6
[7, 2, 6, 5, 3, 9, 4, 10, 8]
0.10011110010001101010101
```

## गहराई में जाएँ:
"रैंडम नंबर जेनरेट करना" की शुरुआत साल 1940 में मार्टिन गंडा और अंजल श्रॉर ने एक मानव जीवन करने तंत्र बनाने के लिए की थी। आज, यह एक प्रमुख हिस्सा बन गया है जिससे हम प्रोग्रामिंग की दुनिया में काम करते हैं। अन्य विकल्प जैसे कि समस्या के लिए अन्य तरीकों का उपयोग करना भी संभव है। इसके अलावा, "रंडम नंबर जेनरेट करना" को आमतौर पर पीडीफ और अन्य इलेक्ट्रॉनिक गेम्स खेलने के लिए भी इस्तेमाल किया जाता है।

## इसके साथ और देखें:
- [कंप्यूटर और उसके यादृच्छिकता: एक इतिहासी अध्ययन](https://www.khanacademy.org/computing/computer-science/cryptography/crypt/v/random-vs-pseudorandom-number-generators)
- [Ruby में स्ट्रिंग और स्ट्रिंग रैंडमाइजेशन](https://www.tutorialspoint.com/ruby/string_rand.htm)
- [पेजिंग एवं स्क्रॉलिंग के साथ यादृच्छिक सूची का उत्पादन करना](https://stackoverflow.com/questions/17016023/growing-randomly-generated-list-with-pagination-and-scrolling)