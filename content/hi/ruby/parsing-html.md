---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:33:41.894322-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTML पार्सिंग यानी वेब पेज के HTML को उसके घटकों में तोड़ना ताकि हम उससे डेटा निकाल सकें। यह जानकारी एकत्रित करने, वेब क्रॉलर बनाने, या डेटा को ऑटोमेटिक तरीके से देखने के लिए किया जाता है।

## How to: (कैसे करें:)
Ruby में HTML पार्स करने के लिए नोकोगिरी जेम बहुत लोकप्रिय है। नोकोगिरी का इस्तेमाल करके आप HTML डॉक्युमेंट से जानकारी निकाल सकते हैं। नीचे एक साधारण उदाहरण दिया गया है:

```ruby
require 'nokogiri'
require 'open-uri'

# HTML डॉक्युमेंट खोलने के लिए
html = open('https://www.example.com')

# Nokogiri का इस्तेमाल करके पार्स करें
doc = Nokogiri::HTML(html)

# हेडर्स निकालें
headers = doc.css('h1, h2, h3').map(&:text)

# आउटपुट प्रिंट करें
puts headers
```

यदि www.example.com पर तीन हेडर्स हैं - "Welcome!", "Learn More", और "Contact Us", तो आउटपुट होगा:
```
Welcome!
Learn More
Contact Us
```

## Deep Dive (गहराई में जानकारी):

HTML पार्सिंग की जड़ें वेब के शुरुआती दिनों में हैं जब ब्राउज़र्स को सादे टेक्स्ट से स्ट्रक्चर्ड पेज बनाना होता था। नोकोगिरी से पहले, Ruby में Hpricot जैसे लाइब्रेरीज़ थी, लेकिन नोकोगिरी की गति और विश्वसनीयता ने इसे प्रधान बना दिया।

वैकल्पिक लाइब्रेरीज में Oga, HTML::Parser, और Mechanize शामिल हैं। इनका उपयोग भी परिस्थितियों के अनुसार होता है। पार्सिंग के दौरान, ध्यान देना होगा कि वेबसाइट की टर्म्स ऑफ सर्विस का उल्लंघन न हो।

## See Also (और जानकारी):

- Nokogiri ऑफिशियल साइट: [Nokogiri Website](https://nokogiri.org/)
- Mechanize जेम: [Mechanize Library](https://github.com/sparklemotion/mechanize)