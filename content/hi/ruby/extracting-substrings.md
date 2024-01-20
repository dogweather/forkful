---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

सबस्ट्रिंग्स निकालना यानी किसी विशेष स्त्रिंग के एक हिस्से को किसी विशेष भाग में निकालना। प्रोग्रामर्स इसका उपयोग तब करते हैं जब उन्हें किसी बड़े डेटा सेट से छोटे टुकड़े को प्राप्त करने की जरूरत होती है।

## कैसे करें:

आइए देखें कि Ruby में सबस्ट्रिंग्स कैसे निकाला जा सकता है।

```Ruby
# एक उदाहरण स्ट्रिंग
str = "नमस्ते, मैं रूबी हूं!"

# इन्डेक्स का उपयोग कर subtring निकालना
sub_str = str[4, 7]
p sub_str
# => "मैं रू"
```

## गहरी दुबकी

Ruby में सबस्ट्रिंग्स निकालने के लिए एकाधिक तरीके हैं जिनमें ""स्ट्रिंग#slice"", ""स्ट्रिंग#[Range]"", ""स्ट्रिंग#split"" आदि शामिल हैं। History के प्रकाश में, यह कार्यकलाप Ruby में स्ट्रिंग्स के साथ काम करने की जड़ी-बूटी है, जिससे प्रोग्रामर कुछ विशेष डेटा को घर्षण रहित ढंग से अलग कर सकते हैं।

## भी देखें:

Ruby सबस्ट्रिंग्स के बारे में और जानने के लिए निम्नलिखित लिंक्स पर जाएँ:

1. Ruby Doc ऑफिशल: [String — Ruby 3.0.2 Documentation] (https://ruby-doc.org/core-3.0.2/String.html)
2. Ruby Guides - [Ruby Substring: How to Extract Strings] (https://www.rubyguides.com/2018/01/ruby-string-methods/).