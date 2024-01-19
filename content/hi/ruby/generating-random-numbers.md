---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

संगणकांक-उत्पादन ("random number generation") संगणना के क्षेत्र में अप्राप्य (अर्थात, अनुमान न करने योग्य) संख्याओं का निर्माण करने की प्रक्रिया है। यह अक्सर उचितता (fairness) और प्रश्नोत्तरी आदि में योग्यता, अनुमानित परिणामों की बेतरतीबी, और मूल्यांकन की यादृच्छिकता को सुनिश्चित करने के लिए प्रोग्रामिंग में उपयोग होता है।

## कैसे करें:

```Ruby
# आवश्यक श्रेणी लोड करें
require 'securerandom'

# 0 और 100 के बीच एक यादृच्छिक संख्या उत्पन्न करें
rand_number = SecureRandom.random_number(100)
puts rand_number
```

उपरोक्त कोड स्निपेट 0 और 100 के बीच की एक यादृच्छिक संख्या उत्पन्न करेगा। यह संख्या हर बार जब आप कोड को चलाते हैं, अलग हो सकती है।

## गहरी दुबकी

रैंडम नंबर जनरेशन का इतिहास संगणक विज्ञान के आरंभिक दिनों से ही शुरू होता है। मूल रैंडम नंबर जनरेटर्स यादृच्छिक घटनाओं, जैसे कि रेडियोएक्टिव प्रक्षेपण, पर आधारित थे। इसके बावजूद, संगणना में "क्रिप्टोग्राफिकली सुरक्षित" प्रायोगिक रैंडम नंबर जनरेटर्स, जैसे कि `SecureRandom`, हमें पर्याप्त "यादृच्छिकता" प्रदान कर सकते हैं।

Ruby में, विकल्प `rand` और `Random.new` भी उपलब्ध हैं, जो आपकी आवश्यकताओं पर निर्भर कर सकते हैं। `rand` जीवनकाल के लिए एक एकल "ग्लोबल" जनरेटर का उपयोग करता है, जबकि `Random.new` हर बार एक नया जनरेटर बनाता है। 

## भी देखें

1. [Ruby डॉक्यूमेंटेशन: Random](https://ruby-doc.org/core-2.7.1/Random.html)
2. [Ruby डॉक्यूमेंटेशन: SecureRandom](https://ruby-doc.org/stdlib-2.5.1/libdoc/securerandom/rdoc/SecureRandom.html)
3. [Random Number Generation in Ruby](https://www.geekhideout.com/rand.rb.shtml)