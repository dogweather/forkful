---
date: 2024-01-20 17:32:31.828383-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Ruby \u092E\u0947\
  \u0902 \u0924\u093E\u0930\u0940\u0916\u0947\u0902 \u0917\u0923\u0928\u093E \u0915\
  \u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948\u0964 \u0906\u0907\u092F\u0947\
  , \u0926\u0947\u0916\u0947\u0902."
lastmod: '2024-03-13T22:44:53.247418-06:00'
model: gpt-4-1106-preview
summary: "Ruby \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916\u0947\u0902 \u0917\
  \u0923\u0928\u093E \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948\u0964\
  \ \u0906\u0907\u092F\u0947, \u0926\u0947\u0916\u0947\u0902."
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
weight: 26
---

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
