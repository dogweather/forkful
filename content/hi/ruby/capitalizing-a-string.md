---
title:                "एक स्ट्रिंग को मजबूत बनाना"
html_title:           "Ruby: एक स्ट्रिंग को मजबूत बनाना"
simple_title:         "एक स्ट्रिंग को मजबूत बनाना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
यदि आप किसी तार को "capitalizing" करते हैं, तो आप प्रत्येक शब्द के पहले अक्षर को बड़ा बना रहे हैं। कार्यक्रमकार इसे यह सुनिश्चित करने के लिए करते हैं कि टेक्स्ट दिखाई दे रहा है, जैसा कि उन्होंने इसे अभिप्रेत किया है - उदाहरण के लिए, शीर्षक में या नये वाक्य की शुरुआत में। 

## कैसे करें:
Ruby में, हमें केवल मूल तार पर `.capitalize` विधि का उपयोग करना होगा। यहाँ एक उदाहरण है:

```Ruby
original_string = "namaste, duniya!"
capitalized_string = original_string.capitalize
puts capitalized_string
```
आउटपुट होगा:
```
Namaste, duniya!
```

## गहरा गोता
`.capitalize` विधि का उपयोग करना Ruby में बहुत सरल है, लेकिन यहाँ कुछ और विवरण हैं:

* ऐतिहासिक संदर्भ: `capitalize` विधि का उपयोग करने वाले अन्य कार्यक्रमण भाषाओं में से अधिकांश (जैसे Python और JavaScript) में Ruby की तुलना में यह भिन्न होता है।

* विकल्प: `titleize` विधि का उपयोग करके हम हर शब्द के पहले अक्षर को बड़ा बना सकते हैं, लेकिन इसे `active_support/inflector` में परिभाषित किया गया है, जो Rails का हिस्सा है।

* कार्यान्वयन विवरण: `capitalize` एक बिल्ट-इन Ruby फ़ंक्शन है जो “destructive” ऐसा नहीं है, जिसका अर्थ है कि यह मूल तार को संशोधित नहीं करता है। यहाँ यह लग रहा है कि कुछ ऐसा हो रहा है जैसा कि:

     ```Ruby
     original_string = original_string.downcase
     original_string[0] = original_string[0].upcase
     ```

## और देखें
अधिकांश भाषाओं में कैपिटलाइज़ करने के विभिन्न तरीकों के बारे में अधिक जानने के लिए, यहाँ कुछ स्रोत हैं:

* [JavaScript's `toUpperCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
* [Python's `capitalize`](https://www.w3schools.com/python/ref_string_capitalize.asp)
* [Ruby's `capitalize`](https://ruby-doc.org/core-2.6/String.html#method-i-capitalize)