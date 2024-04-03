---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u0915\u0948\u0938\u0947: \u0930\u0942\u092C\u0940 [\u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0947\
  \ \u0932\u093F\u090F \u0938\u0940\u0927\u0940 \u0935\u093F\u0927\u093F\u092F\u093E\
  \u0901 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\
  ](https://docs.ruby-lang.org/en/3.3/String.html), \u091C\u093F\u0938\u092E\u0947\
  \u0902 \u0915\u0948\u092A\u093F\u091F\u0932\u093E\u0907\u091C\u0947\u0936\u0928\
  \ \u0936\u093E\u092E\u093F\u0932 \u0939\u0948."
lastmod: '2024-03-25T19:22:37.915032-06:00'
model: gpt-4-0125-preview
summary: "\u0930\u0942\u092C\u0940 [\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0947 \u0932\u093F\u090F \u0938\u0940\
  \u0927\u0940 \u0935\u093F\u0927\u093F\u092F\u093E\u0901 \u092A\u094D\u0930\u0926\
  \u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ \u091C\u093F\u0938\u092E\u0947\u0902 \u0915\u0948\u092A\u093F\u091F\u0932\u093E\
  \u0907\u091C\u0947\u0936\u0928 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948."
title: "\u090F\u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B\
  \ \u092C\u0921\u093C\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\
  \u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 2
---

## कैसे:
रूबी [स्ट्रिंग हेरफेर के लिए सीधी विधियाँ प्रदान करता है](https://docs.ruby-lang.org/en/3.3/String.html), जिसमें कैपिटलाइजेशन शामिल है:

```ruby
# रूबी का बिल्ट-इन तरीका
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

बहुत सुविधाजनक।

रूबी का `.capitalize` तरीका सुविधाजनक है लेकिन केवल पहले अक्षर को अपरकेस में बदलता है। अधिक नियंत्रण के लिए या एक स्ट्रिंग में प्रत्येक शब्द को कैपिटलाइज करने के लिए (जिसे टाइटल केस के रूप में जाना जाता है), आप Rails ActiveSupport एक्सटेंशन से `titleize` विधि का उपयोग करना चाहेंगे, या खुद से लागू करना चाहेंगे:

```ruby
# रेल्स में ActiveSupport के 'titleize' का उपयोग करना
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# होम-मेड समाधान
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

यह विधि स्ट्रिंग को शब्दों की एक सरणी में विभाजित करती है, प्रत्येक को कैपिटलाइज करती है, फिर उन्हें स्पेस के साथ फिर से जोड़ती है।

व्यक्तिगत रूप से, मैं अपने कोड में इस विचार को बहुत आगे ले जाता हूँ। मैंने अपना खुद का [`titleize` विधि लिखी है जो "a" और "the" जैसे छोटे शब्दों को ध्यान में रखती है](https://github.com/public-law/law_string/blob/master/lib/law_string.rb)।
