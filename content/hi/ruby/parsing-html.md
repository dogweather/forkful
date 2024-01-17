---
title:                "Html को पारसिंग करना।"
html_title:           "Ruby: Html को पारसिंग करना।"
simple_title:         "Html को पारसिंग करना।"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## समझना और क्यों?
HTML टैग को पढ़ने और उससे डेटा को निकालने का प्रक्रिया पार्सिंग एक कोडिंग प्रैक्टिस है। यह प्रोग्रामर्स क्योंकि वे भिन्न वेब पृष्ठों से डेटा निकालने के लिए उपयोगी होता है।

## कैसे:
```Ruby
require 'nokogiri'
require 'open-uri'

# HTML पेज से डेटा निकालें
page = Nokogiri::HTML(open("http://www.example.com"))

# हेडलाइन्स को सूची में अपडेट करें
headlines = []
page.css("h1").each do |headline|
  headlines << headline.text.strip
end

# नतीजे प्रिंट करें
puts headlines
```

नतीजा:
```
- प्रथम हेडलाइन
- दूसरा हेडलाइन
- तीसरी हेडलाइन
```

## गहराई की तलाशी:
पहले से ही हम HTML पेजों को पार्स करने के लिए कई उपाय हैं। एक ऐसा उपाय कोडिंग भाषाओं जैसे Python या JavaScript का उपयोग करना है। अन्य उपयोगी उपाय Nokogiri, Beautiful Soup और Scrapy जैसे पार्सिंग लाइब्रेरी भी हैं। HTML पार्सिंग के लिए समय पर ध्यान देना भी महत्वपूर्ण है क्योंकि बड़े पेजों को पार्स करने में देरी हो सकती है।

## और भी देखें:
- [Ruby के लिए Nokogiri दस्तावेज़](http://www.rubydoc.info/github/sparklemotion/nokogiri)
- [Beautiful Soup लाइब्रेरी के लिए विकिपीडिया पृष्ठ](https://en.wikipedia.org/wiki/Beautiful_Soup_(HTML_parser))
- [Scrapy के लिए दस्तावेज़](https://doc.scrapy.org/en/latest/)