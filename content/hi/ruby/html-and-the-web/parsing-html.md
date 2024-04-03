---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:38.283551-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0930\u0942\u092C\
  \u0940 \u092E\u0947\u0902 HTML \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F, 'Nokogiri' \u091C\u0947\u092E \u0915\u094B\
  \ `gem install nokogiri` \u0915\u0947 \u0938\u093E\u0925 \u0938\u094D\u0925\u093E\
  \u092A\u093F\u0924 \u0915\u0930\u0947\u0902\u0964 Nokogiri \u0930\u0942\u092C\u0940\
  \ \u092E\u0947\u0902 HTML \u0914\u0930 XML \u0915\u0947 \u0938\u093E\u0925 \u0915\
  \u093E\u092E \u0915\u0930\u0928\u0947 \u0915\u0947\u2026"
lastmod: '2024-03-13T22:44:53.220209-06:00'
model: gpt-4-0125-preview
summary: "\u0930\u0942\u092C\u0940 \u092E\u0947\u0902 HTML \u092A\u093E\u0930\u094D\
  \u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, 'Nokogiri' \u091C\
  \u0947\u092E \u0915\u094B `gem install nokogiri` \u0915\u0947 \u0938\u093E\u0925\
  \ \u0938\u094D\u0925\u093E\u092A\u093F\u0924 \u0915\u0930\u0947\u0902\u0964 Nokogiri\
  \ \u0930\u0942\u092C\u0940 \u092E\u0947\u0902 HTML \u0914\u0930 XML \u0915\u0947\
  \ \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F \u090F\u0915 \u0938\u094D\u0935\u093F\u0938 \u0906\u0930\u094D\u092E\
  \u0940 \u091A\u093E\u0915\u0942 \u0915\u0940 \u0924\u0930\u0939 \u0939\u0948\u0964\
  \ \u092F\u0939\u093E\u0901 \u090F\u0915 \u0924\u094D\u0935\u0930\u093F\u0924 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे करें:
रूबी में HTML पार्स करने के लिए, 'Nokogiri' जेम को `gem install nokogiri` के साथ स्थापित करें। Nokogiri रूबी में HTML और XML के साथ काम करने के लिए एक स्विस आर्मी चाकू की तरह है। यहाँ एक त्वरित उदाहरण है:

```ruby
require 'nokogiri'
require 'open-uri'

# एक वेबसाइट से HTML सामग्री लोड करें
html_content = URI.open('http://example.com').read

# HTML का पार्सिंग करें
doc = Nokogiri::HTML(html_content)

# शीर्षक निकालें
title = doc.xpath('//title').text
puts "पृष्ठ का शीर्षक है: #{title}"
```

इससे कुछ ऐसा निकलता है: `पृष्ठ का शीर्षक है: Example Domain`.

## गहराई में जानकारी
पुराने रूबी दिनों में, HTML का पार्सिंग के लिए विकल्प सीमित थे। REXML बिल्ट-इन था लेकिन धीमा था। फिर Hpricot आया, लेकिन वह गायब हो गया। Nokogiri 2008 में प्रस्तुत हुआ, Hpricot की आसानी के साथ libxml की गति और शक्ति को मिलाकर, जो कि एक सिद्ध XML टूलकिट है।

पार्सिंग दुनिया में, हमेशा विकल्प होते हैं। कुछ 'rexml' लाइब्रेरी या 'oga', रूबी के लिए एक और XML/HTML पार्सर पर कसम खाते हैं। लेकिन Nokogiri अपनी मजबूती और गति के लिए, ना केवल अपनी विशाल सुविधाओं की सरणी के कारण, एक पसंदीदा बना हुआ है।

भीतर से, Nokogiri HTML को एक दस्तावेज़ वस्तु मॉडल (DOM)—एक वृक्ष संरचना में परिवर्तित करता है। यह तत्वों को नेविगेट और संशोधित करना आसान बनाता है। XPath और CSS सेलेक्टर्स का उपयोग करके, आप जरूरत की किसी भी जानकारी को पिनपॉइंट कर सकते हैं।

## देखें भी
- Nokogiri जेम: [https://nokogiri.org/](https://nokogiri.org/)
- रूबी का rexml दस्तावेज़ीकरण: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- विकल्प पार्सर 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- XPath के बारे में जानें: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
