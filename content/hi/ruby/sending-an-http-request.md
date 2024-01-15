---
title:                "Hindi में http अनुरोध भेजना"
html_title:           "Ruby: Hindi में http अनुरोध भेजना"
simple_title:         "Hindi में http अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
आज कल आप अपने स्मार्टफोन या कंप्यूटर से जितनी भी एप्लिकेशन्स या वेबसाइट्स का इस्तेमाल करते हैं, वे सभी HTTP (HyperText Transfer Protocol) का इस्तेमाल करके काम करते हैं. यह एक कमाल की प्रोटोकॉल है जो आपको अपनी डेटा को अन्य सर्वरों या सर्विसेस के साथ संचार करने में मदद करता है. इसलिए आपको भी HTTP रिक्वेस्ट भेजना आने वाले समय में बहुत ही उपयोगी हो सकता है.

## How To

आप रुबी का इस्तेमाल करके भी आसानी से HTTP रिक्वेस्ट भेज सकते हैं. पहले हम आपको बताएंगे कि कैसे आप आसानी से किसी वेबसाइट पर GET रिक्वेस्ट भेज सकते हैं:

```Ruby
require 'net/http'
uri = URI("https://your-website.com") # Change this to the website you want to send a request to
response = Net::HTTP.get_response(uri)
puts response.body
```

ऊपर दिए गए कोड से आप आसानी से किसी भी वेबसाइट पर GET रिक्वेस्ट भेज सकते हैं और रिस्पॉन्स को अपनी जरूरत के अनुसार प्रिंट कर सकते हैं. अब हम बात करते हैं कि कैसे POST रिक्वेस्ट भेजा जा सकता है:

```Ruby
require 'net/http'
require 'uri'
uri = URI("https://your-website.com") # Change this to the website you want to send a request to
res = Net::HTTP.post_form(uri, 'key1' => 'value1', 'key2' => 'value2') # Change the key-value pairs according to your needs
puts res.body
```

ऊपर दिए गए कोड से आप आसानी से POST रिक्वेस्ट भेज सकते हैं. आपको सिर्फ वेबसाइट का URL और अपने द्वारा भेजने वाले डेटा को बताना होगा. अब हम बात करते हैं कि कैसे आप अपने रिक्वेस्ट में कुछ और हेडर्स और पैरामीटर्स भी डाल सकते हैं:

```Ruby
require 'net/http'
require 'uri'
uri = URI("https://your-website.com") # Change this to the website you want to send