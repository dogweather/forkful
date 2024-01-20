---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? ("What & Why?")
स्ट्रिंग से तारीख पार्स करना मतलब हाश या array में संग्रहित तारीख को पढ़ने और उसे मान्य डेटा टाइप में बदलना होता है। प्रोग्रामर इसे इसलिए करते हैं ताकि वे तारीख से संबंधित कम्प्लेक्स कार्यवाही, जैसे कि समय की गणना, सोर्टिंग या फ़िल्टर करना सकें।

## कैसे करें: ("How to:")
Ruby में आप DateTime पुस्तिका का उपयोग करके स्ट्रिंग से तारीख पार्स कर सकते हैं। 

```Ruby
require 'date'
date_str = '07-09-2021'
parsed_date = Date.parse(date_str)
puts parsed_date
```

ऊपरी कोड का आउटपुट निम्नलिखित होगा:

```Ruby
# 2021-07-09
```

## गहरा दिवे ("Deep Dive")

रूबी में Date.parse विधि का इतिहास 1990 के दशक के बाद शुरू हुआ। यह विधि मालूम करने में मदद करती है कि स्ट्रिंग तारीख में कौन सा होना चाहिए। यदि स्ट्रिंग युरोपीय तारीख फ़ार्मेट (दिन-महिना-वर्ष) का उपयोग करता है, तो Date.parse विधि युरोपीय फ़ार्मेट का अनुमान लगाएगी। 

पार्स करने का अन्य तरीका है Date.strptime इसका उपयोग तब किया जाता है जब आप तारीख के फ़ार्मेट का पता रखते हैं।

```Ruby
require 'date'
date_str = '07-09-2021'
parsed_date = Date.strptime(date_str, '%d-%m-%Y')
puts parsed_date
```

## देखें भी ("See Also")

[Date.parse डॉक्युमेंटेशन](https://ruby-doc.org/stdlib-2.6.1/libdoc/date/rdoc/Date.html#method-c-parse)
[Date.strptime डॉक्युमेंटेशन](https://ruby-doc.org/stdlib-2.6.1/libdoc/date/rdoc/Date.html#method-c-strptime)
[Ruby तारीख और समय गाइड](https://www.rubyguides.com/2015/12/ruby-time/)