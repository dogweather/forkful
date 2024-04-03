---
date: 2024-01-20 17:58:49.401667-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:53.194493-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

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
