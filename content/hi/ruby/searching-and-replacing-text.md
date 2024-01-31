---
title:                "पाठ खोजना और बदलना"
date:                  2024-01-20T17:58:49.401667-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट सर्च और रिप्लेस का मतलब है किसी डॉक्यूमेंट में विशेष शब्द या फ्रेज को ढूँढकर उसे नए शब्दों से बदलना। प्रोग्रामर इसलिए करते हैं क्योंकि कोड में त्रुटियों को सुधारना, नाम बदलना या कोड अपडेट करना होता है।

## How to: (कैसे करें:) 

```Ruby
# एक स्ट्रिंग में सब 'dog' को 'cat' से रिप्लेस करना
text = "The quick brown dog jumps over the lazy dog"
new_text = text.gsub('dog', 'cat')

puts new_text # => The quick brown cat jumps over the lazy cat
```

```Ruby
# मैच करने वाले पैटर्न के साथ रिप्लेसमेंट यूज करना
text = "The quick brown fox jumps over the 12 lazy dogs"
new_text = text.gsub(/(\d+)\s+lazy\s+(\w+)/, 'active \1 and well-fed \2')

puts new_text # => The quick brown fox jumps over the active 12 and well-fed dogs
```

## Deep Dive (गहराई में जानकारी):

सर्च और रिप्लेस फंक्शन से स्ट्रिंग में बदलाव आसानी से हो जाते हैं। `gsub` मेथड रेगुलर एक्सप्रेशन्स को सपोर्ट करता है, जो कॉम्प्लेक्स पैटर्न्स की खोज करते हैं। ये सुविधा शुरुआती दिनों से ही रूबी में है, Perl से प्रेरित होकर जिसे पावरफुल टेक्स्ट प्रोसेसिंग के लिए डिजाइन किया गया था। `sub` मेथड भी है जो केवल पहली इंस्टेंस को रिप्लेस करता है। 

स्ट्रिंग प्रोसेसिंग परफॉरमेंस पर असर डाल सकती है, इसलिए हमेशा रेगेक्स पैटर्न्स को इफेक्टिवली लिखना चाहिए।

## See Also (और देखें):

- Ruby का डॉक्यूमेंटेशन [gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub) और [sub](https://ruby-doc.org/core-2.7.1/String.html#method-i-sub) मेथड्स के लिए।
- [Regexp](https://ruby-doc.org/core-2.7.1/Regexp.html) क्लास: यह जानने के लिए कि रेगुलर एक्सप्रेशन्स कैसे काम करते हैं।
- [Rubular](http://rubular.com): रेगुलर एक्सप्रेशन्स को टेस्ट करने के लिए एक रूबी-आधारित टूल।
- "Effective Ruby" किताब – स्ट्रिंग प्रोसेसिंग और अन्य टॉपिक्स पर महत्वपूर्ण टिप्स।
