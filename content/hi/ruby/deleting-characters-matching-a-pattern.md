---
title:                "पैटर्न को मिलते हुए अक्षरों को हटाना"
html_title:           "Ruby: पैटर्न को मिलते हुए अक्षरों को हटाना"
simple_title:         "पैटर्न को मिलते हुए अक्षरों को हटाना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप Ruby में प्रोग्राम लिखते हैं और आपको एक स्ट्रिंग से चर जो मिलते हैं एक निशान (pattern) के साथ हटाना हो तो आप `gsub` मेथड का उपयोग कर सकते हैं। इससे आपको वह स्ट्रिंग मिलेगी जो अर्पित निशान को हटाकर बनी होती है।

## कैसे करें

आप `gsub` मेथड का उपयोग सुनिश्चित करने के लिए a नामित प्रॉपर्टी से स्क्रिप्ट को पास कर सकते हैं।

```Ruby
str = "Hello, world!"
new_str = str.gsub(/[eo]/, '')
puts str
puts new_str
```

आउटपुट:

```
Hello, world!
Hll, wrld!
```

## गहराई में जाएं

यह method बहुत उपयोगी है जब आपको बहुत सारे चर नहीं परंतु जिनकी एक पदार्थ.pattern] को पूरा करते हैं इसलिए आपको इससे नुकसान मिलता है। इसमें *regex* (regular expression) का उपयोग किया जा सकता है, जो आपको आसानी से इस्तेमाल करने देता है।

## देखे भी

[Regular Expressions in Ruby](https://www.regular-expressions.info/ruby.html)
[Ruby's gsub Method](https://www.rubyguides.com/2019/12/gsub-method-in-ruby/)