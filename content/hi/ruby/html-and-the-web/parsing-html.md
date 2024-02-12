---
title:                "HTML विश्लेषण"
aliases: - /hi/ruby/parsing-html.md
date:                  2024-02-03T19:13:38.283551-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML का पार्सिंग मतलब HTML कोड के एक टुकड़े को अलग करके उसकी संरचना और सामग्री को समझना। प्रोग्रामर इसे डेटा निकालने, सामग्री को संशोधित करने, या जानकारी को विभिन्न प्रारूपों और प्रणालियों के बीच स्थानांतरित करने के लिए करते हैं।

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
