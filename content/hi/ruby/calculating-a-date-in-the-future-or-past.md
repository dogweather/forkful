---
title:                "भविष्य या अतीत में तारीख की गणना"
aliases:
- hi/ruby/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:31.828383-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख की गणना करना मतलब भविष्य या अतीत की कोई निश्चित तारीख तक पहुँचना। प्रोग्रामर्स इसलिए करते हैं क्योंकि कभी-कभी अनुप्रयोग को प्रोजेक्ट डेडलाइन, बिल की अवधियाँ, या उपयोगकर्ता की गतिविधियों का हिसाब रखना पड़ता है।

## कैसे करें:

Ruby में तारीखें गणना करना सरल है। आइये, देखें:

```Ruby
require 'date'

# आज की तारीख
today = Date.today
puts "आज की तारीख: #{today}"

# 10 दिन बाद की तारीख
future_date = today + 10
puts "10 दिन बाद की तारीख: #{future_date}"

# 20 दिन पहले की तारीख
past_date = today - 20
puts "20 दिन पहले की तारीख: #{past_date}"
```

यह कोड तीन तारीखें दिखाएगा: आज, आज से 10 दिन बाद और आज से 20 दिन पहले।

## गहराई से जानकारी

तारीख की गणना अक्सर `Date` और `Time` क्लासेज़ के साथ की जाती है। Ruby 1.9 के बाद से, हमारे पास improved `Date` और `DateTime` क्लासेज़ हैं। फिर भी, जटिल समय संबंधी गणना के लिए हम `time` स्टैंडर्ड लाइब्रेरी या gems जैसे की `active_support` का उपयोग कर सकते हैं। यह गणना टाइमज़ोन्स और लीप ईयर्स को भी ध्यान में रखती है। 

वैकल्पिक रूप से, प्रोग्रामर्स `chronic` गेम का भी उपयोग कर सकते हैं जो तारीखों की और अधिक स्वाभाविक गणना देता है। 

## अन्य संबंधित सूत्र

- Ruby की अधिकृत डॉक्युमेंटेशन: [Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html) और [Time](https://ruby-doc.org/core-3.0.0/Time.html)
- RubyGems पर `active_support` gem: [Active Support](https://rubygems.org/gems/activesupport)
- RubyGems पर `chronic` gem: [Chronic](https://rubygems.org/gems/chronic)
