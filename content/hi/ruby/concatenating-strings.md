---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वचनों को जोड़ने का अर्थ होता है दो या दो से अधिक स्ट्रिंग्स को एकत्र करना। प्रोग्रामर्स इसे करते हैं क्योंकि इससे उन हें डायनेमिक स्ट्रिंग्स बनाने का विकल्प मिल जाता है, जिससे कोड को पढ़ना और लिखना आसान हो जाता है।

## कैसे करें:

```Ruby
#स्ट्रिंग्स जोड़ने का बहुत सरल तरीका है
greeting = "नमसतें, "
name = "मोहन"
message = greeting + name
puts message
# => "नमस्ते, मोहन”
```
## गहराई की जाँच :
हम जब रूबी का इस्तेमाल करते हैं तो स्ट्रिंग्स को जोड़ने के लिए कई विकल्प होते हैं। उपरोक्त "+" ऑपरेटर के अलावा, आप "<<" या "concat" का भी उपयोग कर सकते हैं। यह हमेशा आवश्यक नहीं है कि "+" ऑपरेटर ही हमेशा सर्वश्रेष्ठ हो, परिस्थितियों के आधार पर, आपको अन्य विकल्पों का भी उपयोग करना चाहिए।

## यह भी देखें :
[Ruby String Concatenation | Stack Overflow](https://stackoverflow.com/questions/4684446/why-is-the-shovel-operator-preferred-over-plus-equals-when-building-a)
[Ruby String Concatenation | Ruby Learning](https://rubylearning.com/satishtalim/ruby_string_concatenation.html)