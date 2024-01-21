---
title:                "स्ट्रिंग इंटरपोलेशन"
date:                  2024-01-20T17:52:20.538518-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

स्ट्रिंग इंटरपोलेशन यानी कि एक स्ट्रिंग के बीच में वेरिएबल्स की वैल्यू या किसी एक्सप्रेशन का रिजल्ट डालना। यह करने से कोड साफ और फ्लेक्सिबल रहता है, और डायनेमिक स्ट्रिंग्स आसानी से बनाई जा सकती हैं।

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