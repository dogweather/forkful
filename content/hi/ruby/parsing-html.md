---
title:                "एचटीएमएल का अनुवाद करना"
html_title:           "Ruby: एचटीएमएल का अनुवाद करना"
simple_title:         "एचटीएमएल का अनुवाद करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## हमें क्यों पार्सिंग HTML करना चाहिए?

HTML प्रोग्रामिंग में बहुत महत्वपूर्ण है क्योंकि यह वेब पेज का स्ट्रक्चर और दिखाने का तरीका बताता है। पार्सिंग HTML समझने के लिए, आपको अपनी वेब साइट पर जाने की आवश्यकता होती है और उस पर सामग्री को समझने के लिए आपको इसे पहचानना चाहिए।

## कैसे करें

``` Ruby
require 'nokogiri'
require 'open-uri'

url = "https://www.example.com"
data = Nokogiri::HTML(open(url))

puts data.title
puts data.css("h1").text
```

इस कोड से, आप वेब साइट के दृश्यमान संचार को पढ़ सकते हैं, जैसे कि शीर्षक और शीर्षक 1 का पाठ। इस तरह से, आप इस के साथ और अधिक एक वेब साइट के विभिन्न भाग डाल सकते हैं और उससे संग्रहकों की खोज और विवरण प्राप्त कर सकते हैं।

## गहराई में जाओ

HTML पार्सिंग के लिए, हम Nokogiri और open-uri जैसी पुस्तकालयों का उपयोग करते हैं। Nokogiri आसानी से HTML टैग को खोजने और पढ़ने की अनुमति देता है, जबकि open-uri वेब पृष्ठों को खोलकर डेटा को देर तक लाता है। आप अपनी स्क्रिप्ट को थरोलीक्स और stoleक जैसे अवतरणों के साथ अनुकूलित कर सकते हैं।

## देखें भी

- [Nokogiri दस्तावेज़ीकरण] (https://nokogiri.org/)
- [एचटीएमएल बाइट एक्सपर्ट नोटेशन] (https://www.w3schools.com/html/html_entities.asp)
- [ओपन-यूआरआई दस्तावेज़ीकरण] (https://github.com/NoamB/sorbet-open-uri#-open-uri)