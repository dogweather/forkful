---
title:                "उपस्थित स्ट्रिंगस का निकास करना"
html_title:           "Ruby: उपस्थित स्ट्रिंगस का निकास करना"
simple_title:         "उपस्थित स्ट्रिंगस का निकास करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## वो क्या है और क्यों करते हैं?
वस्तुस्थ, एक substring का अर्थ है कोई शब्द, अक्षर या भाषा के कुछ भाग को अलग करना। ये सिर्फ कुछ शब्दों के सिर्फ उपभाग हो सकते हैं जो एक पूर्ण शब्द का अविभाज्य भाग हैं। प्रोग्राम करने वाले लोग substring को अक्सर उनके काम में प्रयोग करते हैं क्योंकि लगभग हर कोड में इसका उपयोग हो सकता है।

## कैसे:
```Ruby
str = "Hello World"
puts str[0,5] # Output: Hello
puts str[6,5] # Output: World
```
यहां, ```str[0,5]``` एक substring है जिसमें शब्द नंबर 0 से 4 तक होते हैं। अतः, पहले 5 अक्षर अलग किए गए हैं। इसी तरह, ```str[6,5]``` अन्य substring है जिसमें शब्द नंबर 6 से 10 तक होते हैं। अतः, अगले 5 अक्षर अलग किए गए हैं।

## गहराई में:
तारीख के मामले में, substring का उपयोग शुरू में text editors में शुरु हुआ जहां कप्यूटर जीवित किया जाता था जब ये operate करता था। आजकल, substring आपको array methods में संभावित करता है, प्रमाणित फंक्शनों, और अधिक। substring प्रयोग करने के लिए कुछ अलग तरह से आप text processing API से प्रभावी रूप से किया जा सकता है।

## इससे जुड़े हुए लिंक:
- [Ruby की वर्तमान संस्करण](https://www.ruby-lang.org/hi/)
- [Learn Ruby in Y minutes](https://learnxinyminutes.com/docs/hi-in/ruby/)
- [Substring के बारे में सब कुछ](https://www.rubyguides.com/2018/10/ruby-substring/)