---
date: 2024-01-20 17:52:20.538518-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:55.158231-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to: (कैसे करें:)
```ruby
# एक साधारण इंटरपोलेशन उदाहरण
name = 'राज'
welcome_message = "नमस्ते, #{name}!"
puts welcome_message
# आउटपुट: नमस्ते, राज!

# संख्याओं के साथ इंटरपोलेशन
age = 30
message = "तुम #{age} साल के हो।"
puts message
# आउटपुट: तुम 30 साल के हो।

# एक्सप्रेशन के साथ इंटरपोलेशन
hours = 24
day_message = "एक दिन में #{hours * 60} मिनट होते हैं।"
puts day_message
# आउटपुट: एक दिन में 1440 मिनट होते हैं।
```

## Deep Dive (गहराई से जानकारी)
स्ट्रिंग इंटरपोलेशन की शुरूआत Ruby के प्रारंभिक वर्जन्स से ही हो गई थी क्योंकि यह डायनेमिक स्ट्रिंग बनाने का एक शक्तिशाली तरीका है। सीधे `"#{}"` इस्तेमाल करके हम वेरिएबल, एक्सप्रेशन, या किसी भी Ruby कोड को स्ट्रिंग के अंदर चला सकते हैं।

वैकल्पिक तरीकों में `+` ऑपरेटर और `String#concat` मेथड शामिल हैं, लेकिन ये इतने फ्लेक्सिबल और साफ नहीं होते। इंटरपोलेशन में यदि किसी वेरिएबल की वैल्यू `nil` है, तो Ruby उसे एम्प्टी स्ट्रिंग `""` के तौर पर प्रिंट कर देगी। इंटरपोलेशन के दौरान स्ट्रिंग्स ऑटोमेटिकली `.to_s` मेथड से गुजरती है, जिससे नॉन-स्ट्रिंग वैल्यूज़ को भी स्ट्रिंग में बदला जा सकता है।

## See Also (और भी देखें)
- [Ruby Documentation for Strings](https://ruby-doc.org/core-3.1.2/String.html)
- [Ruby Style Guide](https://rubystyle.guide/#string-interpolation)
- [The Well-Grounded Rubyist by David A. Black](https://www.manning.com/books/the-well-grounded-rubyist)
