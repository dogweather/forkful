---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Ruby में, string को capitalize करने का मतलब है कि string के पहले अक्षर को बड़े (uppercase) अक्षर में बदलना। Programmers इसे इसलिए करते हैं क्योंकि कभी-कभी डेटा को प्रदर्शित करते समय पहले अक्षर को बड़े रूप में दिखाना आवश्यक होता है, जैसे कि नाम या शीर्षक में।

## How to (कैसे करें):
```Ruby
# Example 1
string1 = "namaste duniya"
capitalized_string1 = string1.capitalize
puts capitalized_string1  # Output: "Namaste duniya"

# Example 2
string2 = "ruby programming"
capitalized_string2 = string2.split.map(&:capitalize).join(' ')
puts capitalized_string2  # Output: "Ruby Programming"
```

## Deep Dive (गहराई से जानकारी):
पहले के Ruby versions में `.capitalize` का उपयोग करके एक स्ट्रिंग का पहला अक्षर बड़ा किया जाता था। हालांकि, `.capitalize` मेथड सिर्फ पहले शब्द का पहला अक्षर बड़ा करता है, न कि पूरी स्ट्रिंग के हर शब्द का। जैसे कि आप Example 2 में देख सकते हैं, अगर आप पूरी string के हर शब्द को capitalize करना चाहते हैं, तो आप `split`, `map`, और `join` मेथड्स का इस्तेमाल कर सकते हैं।

रूबी में `.downcase` और `.upcase` जैसे alternative methods भी हैं, जो स्ट्रिंग के सभी अक्षरों को क्रमश: छोटे या बड़े रूप में बदल देते हैं।

स्ट्रिंग को capitalize करने का implementation आमतौर पर आसान होता है, लेकिन जब भी बहुभाषी डेटा के साथ काम कर रहे हों, तो Unicode और अन्य encoding issues का ध्यान रखना चाहिए।

## See Also (और भी जानकारी):
- Ruby documentation on String: [Ruby String Capitalize](https://ruby-doc.org/core-3.1.2/String.html#method-i-capitalize)
- Ruby String methods: [Ruby-Doc String Methods](https://ruby-doc.org/core-3.1.2/String.html)