---
title:                "स्ट्रिंग को कैपिटलाइज करना"
date:                  2024-03-25T17:32:54.328375-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग को कैपिटलाइज़ करना आमतौर पर उसके पहले अक्षर को अपरकेस में बदलने और बाकी को लोअरकेस में बदलने का अर्थ रखता है। लेकिन कभी-कभी यह केवल पहले अक्षर को अपरकेस में सुनिश्चित करने का अर्थ रख सकता है, जबकि बाकी स्ट्रिंग को बिना बदले छोड़ देता है। सच कहूँ तो, मेरी राय में, यह एक कुछ हद तक अस्पष्ट शब्द है।

## कैसे:
रूबी, स्ट्रिंग संभाल के लिए [सरल तरीके प्रदान करता है](https://docs.ruby-lang.org/en/3.3/String.html), जिसमें कैपिटलाइज़ेशन भी शामिल है:

```ruby
# रूबी का बिल्ट-इन तरीका
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

बहुत सुविधाजनक।

रूबी का `.capitalize` मेथड आसान है लेकिन केवल पहले अक्षर को अपरकेस बनाता है। अधिक नियंत्रण के लिए या स्ट्रिंग में प्रत्येक शब्द को कैपिटलाइज करने के लिए (जिसे टाइटल केस के रूप में जाना जाता है), आप Rails ActiveSupport एक्सटेंशन से `titleize` मेथड का उपयोग करना चाह सकते हैं, या इसे स्वयं लागू करना चाह सकते हैं:

```ruby
# रेल्स में ActiveSupport का 'titleize' उपयोग करना
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# घर का बना समाधान
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

यह विधि स्ट्रिंग को शब्दों की एक सरणी में विभाजित करती है, प्रत्येक शब्द को कैपिटलाइज़ करती है, फिर उन्हें एक स्थान के साथ फिर से जोड़ती है।

व्यक्तिगत रूप से, मैं अपने कोड में इस विचार को बहुत आगे ले जाता हूँ। मैंने अपनी खुद की [`titleize` मेथड लिखी है जो "a" और "the" जैसे छोटे शब्दों को शामिल करती है](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
