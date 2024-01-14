---
title:                "Ruby: Html को विश्लेषित करना"
simple_title:         "Html को विश्लेषित करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

एचटीएमएल को पार्स करने का काम बहुत उपयोगी हो सकता है, जैसे कि डेटा साइंटिस्ट्स और डेवलपर्स वेब स्क्रैपिंग के लिए इस्तेमाल करते हैं।

## कैसे करें

```Ruby
# एचटीएमएल पार्सिंग कोड का उदाहरण
require 'nokogiri'
require 'open-uri'

page = Nokogiri::HTML(open("https://example.com"))

# CSS सेलेक्टर के माध्यम से एलिमैंट्स को लोकेट करें
page.css('h1').each do |element|
  puts element.text
end
```

इस उदाहरण में, हमने Nokogiri और open-uri जैसे गेमस इन रेल जूल्स का उपयोग करके एचटीएमएल से पाठ को आसानी से परस करके हमनेai

## गहराई में लिखते हैं

एचटीएमएल का पार्सिंग करने के लिए और भी बहुत सारे उपाय हैं, जैसे कि डोक्यूमेंट डोमेन कहा से हम CSS या XPath सेलेक्टर इस्तेमाल करके एलिमैंट्स को लोकेट कर सकते हैं। हमेशा ध्यान रखें कि एचटीएमएल को पार्स करने के लिए समय-समय पर विभिन्न टूलज और लांग्वेजेस समझना जरूरी है।

## और

[हमारा लिंक्स का संकलन](https://www.rubyguides.com/2018/11/parsing-html-ruby/)

[Open-uri रीडमी दस्तावेज़](https://github.com/ruby/ruby/blob/ruby_2_5/lib/open-uri.rb)

[Nokogiri सिक्कों पत्तियाँ](https://www.nokogiri.org/tutorials/searching_a_xml_html_document.html)