---
title:                "टेक्स्ट फाइल पढ़ना"
html_title:           "Ruby: टेक्स्ट फाइल पढ़ना"
simple_title:         "टेक्स्ट फाइल पढ़ना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप रूबी में अपनी प्रोग्राम बनाना चाहते हैं, तो आपको अक्सर टेक्स्ट फ़ाइल का उपयोग करके डेटा पढ़ने या लिखने की आवश्यकता हो सकती है। इस लेख में, हम आपको रूबी के माध्यम से टेक्स्ट फ़ाइल पढ़ने के तरीके बताएंगे।

## कैसे करें

```Ruby
file = File.open("example.txt") # एक नया फ़ाइल खोलें
data = file.read # फ़ाइल से डेटा पढ़ें
puts data # पढ़ा हुआ डेटा प्रिंट करें
```

उपरोक्त कोड का उपयोग करके आप एक टेक्स्ट फ़ाइल को खोल सकते हैं और उसमे से डेटा पढ़ सकते हैं। आप इस तरह से कोड में परिवर्तन करके फ़ाइल के स्ट्रिंग के साथ अन्य काम भी कर सकते हैं।

## डीप डाइव

जब हम `File.open()` विधि को उपयोग करते हैं, तो हम तीन पैरामीटर पास करते हैं - फ़ाइल का नाम, तालिका (read, write, कर्य पारित आदि) और एक ब्लॉक। यहां ब्लॉक में `file` वेरिएबल होगा जो फ़ाइल को होल करेगा। इस तरह से, अगर हमें अन्य कोड को फ़ाइल के साथ काम करना होता है, तो हम इस ब्लॉक में उसे लिख सकते हैं।

## देखें भी

- [Essential Ruby Concepts (Hindi)](https://www.thoughtco.com/intro-to-ruby-for-programming-beginners-4054080)
- [Ruby Documentation (Hindi)](https://ruby-doc.org/)
- [Codeacademy: Learn Ruby (Hindi)](https://www.codecademy.com/learn/learn-ruby)