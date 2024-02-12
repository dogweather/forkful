---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases:
- /hi/ruby/capitalizing-a-string/
date:                  2024-02-03T19:06:59.946911-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में एक स्ट्रिंग को कैपिटलाइज़ करना अक्सर पहले अक्षर को अपरकेस में परिवर्तित करने और शेष को लोअरकेस में परिवर्तित करने का संदर्भ देता है। प्रोग्रामर नामकरण संविधानों का पालन करने, आउटपुट्स को अधिक पठनीय बनाने या तुलना और संग्रहण के लिए डेटा संगति सुनिश्चित करने जैसे कारणों से ऐसा करते हैं।

## कैसे करें:
रूबी स्ट्रिंग हेरफेर के लिए सीधे तरीके प्रदान करता है, जिसमें कैपिटलाइजेशन भी शामिल है। यहाँ पर आप रूबी में एक स्ट्रिंग को कैपिटलाइज कैसे कर सकते हैं:

```ruby
# रूबी का बिल्ट-इन मेथड
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

रूबी का `.capitalize` मेथड सुविधाजनक है लेकिन केवल पहले अक्षर पर प्रभाव डालता है। अधिक नियंत्रण के लिए या एक स्ट्रिंग में प्रत्येक शब्द को कैपिटलाइज़ करने (जिसे टाइटल केस कहते हैं) के लिए, आप Rails ActiveSupport एक्सटेंशन से `titleize` मेथड का उपयोग करना चाहेंगे या इसे स्वयं लागू करना चाहेंगे:

```ruby
# Rails में ActiveSupport का 'titleize' का उपयोग करना
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

यदि आप Rails का उपयोग नहीं कर रहे हैं या एक शुद्ध रूबी समाधान पसंद करते हैं, तो यहाँ पर आप कैसे एक स्ट्रिंग में प्रत्येक शब्द को कैपिटलाइज़ कर सकते हैं:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

यह विधि स्ट्रिंग को शब्दों की एक सरणी में विभाजित करती है, प्रत्येक को कैपिटलाइज़ करती है, फिर उन्हें एक स्पेस के साथ फिर से जोड़ती है।
