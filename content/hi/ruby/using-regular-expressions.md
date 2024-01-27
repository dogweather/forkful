---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रेग्युलर एक्सप्रेशंस (Regular Expressions) पैटर्न मिलान का एक तरीका है। प्रोग्रामर्स इसका इस्तेमाल टेक्स्ट सर्चिंग, वैलिडेशन और परिवर्धन के लिए करते हैं।

## कैसे करें:
```Ruby
# ईमेल वैलिडेशन के लिए एक सिंपल रेगेक्स पैटर्न
email_pattern = /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i
puts email_pattern.match?("user@example.com")  # true

# टेक्स्ट में डिजिट्स खोजना
text = "मेरा नंबर 12345 है।"
digits_pattern = /\d+/
puts text.scan(digits_pattern)  # ["12345"]

# शब्दों की गिनती
words_pattern = /\b[\w']+\b/
sentence = "यहाँ 5 शब्द हैं।"
puts sentence.scan(words_pattern).size  # 5
```

## गहराई से:
रेग्युलर एक्सप्रेशंस का इतिहास 1950 के दशक में शुरू हुआ था जब स्टीफन क्लीन ने उन्हें परिभाषित किया। रेगेक्स के विकल्प में पार्सर्स और स्ट्रिंग मैनिपुलेशन फंक्शंस आते हैं, पर रेगेक्स तेज़ और संक्षिप्त होते हैं। Ruby में, रेगेक्स ऑबजेक्ट्स मल्टीलाइन मोड, इग्नोर-केस मोड जैसे फीचर्स के साथ आते हैं।

## संबंधित सूचना स्रोत:
- [RubyDoc Regexp Class](https://ruby-doc.org/core/Regexp.html)
- [Rubular: एक Ruby रेगेक्स एडिटर](http://rubular.com/)
