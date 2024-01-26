---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:37.611873-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर जेनरेट करना मतलब है अनिश्चितांक संख्याओं का उत्पादन। प्रोग्रामर्स इसे खेलों, आंकड़ा विश्लेषण, सुरक्षा प्रणालियों, और टेस्टिंग में उपयोग करते हैं।

## How to: (कैसे करें:)
```Gleam
import gleam/io
import gleam/random

fn main() {
  let seed = random.default_seed()
  let gen = random.float(seed)
  io.println(gen)
}
```

ऊपर दिये कोड का आउटपुट होगा एक रैंडम नंबर 0.0 और 1.0 के बीच में। हर बार चलाने पर एक नया नंबर मिलेगा।

## Deep Dive (गहन जानकारी)
रैंडम नंबर जेनरेट करनेकी तकनीक कई दशकों से विकसित हो रही हैं। शुरुआत में, इस्तेमाल होते थे साधारण एल्गोरिदम्स जैसे कि लिनियर कंजरेंशियल जेनरेटर्स (LCGs). फिर आए और भी जटिल एल्गोरिदम्स जैसे कि मर्सेन ट्विस्टर। आधुनिक रैंडम नंबर जेनरेटर्स, जिनका उपयोग Gleam जैसी भाषाओं में होता है, वो और भी अधिक विश्वसनीय और अप्रत्याशित नंबर पैदा करते हैं। इनके और इनके अंशांकन (सीडिंग) पर व्यापक अध्ययन हुए हैं ताकि पूर्वानुमान लगाना मुश्किल हो।

रैंडमनेस के विकल्पों में शामिल हैं प्सेडोरैंडम नंबर जेनरेटर्स (PRNGs) और ट्रू रैंडम नंबर जेनरेटर्स (TRNGs)। PRNGs काफी प्रचलित है, जबकि TRNGs हार्डवेयर-आधारित होते हैं और फिजिकल घटनाओं का उपयोग करके असली रैंडमनेस प्रदान करते हैं।

## See Also (और देखें)
- रैंडम नंबर जेनरेटर्स का इतिहास: [Random number generation history](https://en.wikipedia.org/wiki/Random_number_generation)
- रैंडमनेस पर प्रोग्रामिंग: [Introduction to randomness in programming](https://www.random.org/randomness/)
