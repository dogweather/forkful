---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को लोअर केस में बदलना मतलब किसी भी अक्षर को उसके लोअरकेस संस्करण में परिवर्तित करना। प्रोग्रामर्स इसे करते हैं क्योंकि यह स्ट्रिंग की तुलना और हैंडलिंग को सरल और प्रभावी बनाता है।

## कैसे करें:

Ruby में, हम `downcase` method का उपयोग करके किसी स्ट्रिंग को निचले मामले में बदल सकते हैं:

```ruby
str = "Hello World"
lowercase_str = str.downcase
puts lowercase_str
```
आउटपुट:

```ruby
"hello world"
```

## गहरा डाइव

1. ऐतिहासिक संदर्भ: `downcase` मेथड का प्रमुख उपयोग केस-सेंसेटिव तुलनाओं को सरल बनाने के लिए किया जाता है।

2. विकल्प: आज भी अन्य भाषाओं में केस-इंसेंसिटिव तुलनाओं के लिए उपयोग किए जाने वाले कुछ विधियां हैं, जैसे जावास्क्रिप्ट में `toLowerCase()`.

3. क्रियान्वयन विवरण: `downcase` मेथड बिना स्ट्रिंग को संशोधित किए एक नई कॉपी बनाता है जो सम्पूर्ण रूप से लोअरकेस होती है। 

## यह संसाधन भी देखें

- [रूबी डॉक्स](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)