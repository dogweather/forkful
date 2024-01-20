---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# रूबी से HTML पार्स करना : एक त्वरित पत्रिका 

## क्या और क्यों?

HTML पार्स करना होता है वेबपेज का HTML कोड पढ़ना और बोध करना। इसकी आवश्यकता वेब स्क्रेपिंग (वेबपेज से जानकारी निकालने) में होती है।

## कैसे:

```Ruby
require 'open-uri'
require 'nokogiri'

url = 'http://नमूना.वेबसाइट'
html = open(url)

doc = Nokogiri::HTML(html)

# शीर्षक प्राप्त करना
title = doc.at_css('title').text
puts title
```

ऊपरी कोड एक वेबपेज से HTML पार्स करने और शीर्षक प्राप्त करने के लिए है।

## गहरा डाइव:

HTML पार्स करने का इतिहास वेब स्क्रेपिंग के साथ ही शुरु होता है। बदलते समय के साथ, विभिन्न भाषाओं और फ्रेमवर्क में विभिन्न तरीके उभरे हैं।

Ruby में, 'Nokogiri' एक बहुत ही लोकप्रिय और शक्तिशाली लाइब्रेरी है, जिसे HTML पार्स करने के लिए इस्तेमाल किया जाता है।

'Nokogiri' का उपयोग 'open-uri' लाइब्रेरी के साथ मिलकर HTML को पार्स करने में किया जाता है।

## देखिए भी:

1. Nokogiri ट्यूटोरियल: https://nokogiri.org/tutorials/
2. 'open-uri' डॉक्युमेंटेशन: https://ruby-doc.org/stdlib/libdoc/open_uri/rdoc/OpenURI.html
3. Web scraping से संबंधित इस महत्वपूर्ण लेख को deekhein: https://realpython.com/beautiful-soup-web-scraper-python/