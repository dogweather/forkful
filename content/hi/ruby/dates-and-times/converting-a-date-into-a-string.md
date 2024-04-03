---
date: 2024-01-20 17:37:49.354835-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) ."
lastmod: '2024-03-13T22:44:53.244056-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to: (कैसे करें?)
```Ruby
require 'date'

# आज की तारीख प्राप्त करें
today = Date.today

# डिफॉल्ट स्ट्रिंग फॉर्मेट में कन्वर्ट करें
date_string_default = today.to_s

puts date_string_default # "YYYY-MM-DD" फॉर्मेट में प्रिंट होगा

# अपनी पसंद का फॉर्मेट चुनें
date_string_formatted = today.strftime('%d/%m/%Y')

puts date_string_formatted # "DD/MM/YYYY" फॉर्मेट में प्रिंट होगा
```

## Deep Dive (गहराई में जानकारी)
Ruby में डेट को स्ट्रिंग में परिवर्तित करना `Date` और `Time` क्लासेस का इस्तेमाल करते हुए किया जाता है। `strftime` मेथड सबसे उपयोगी है, जो विभिन्न प्रकार के फॉर्मेटिंग विकल्प प्रदान करता है। यह UNIX सिस्टेम में देखे गए `strftime` फंक्शन से प्रेरित है। विकल्प के रूप में, `to_s` मेथड ISO 8601 फॉर्मेट में डेट को डिफॉल्ट स्ट्रिंग फॉर्मेट में बदलता है। अगर कस्टमाईज्ड स्ट्रिंग फॉर्मेट चाहिए हो तो, `strftime` के कई डायरेक्टिव्स का इस्तेमाल कर सकते हैं, जैसे की `%d` दिन के लिए, `%m` महीने के लिए और `%Y` साल के लिए।

## See Also (और जानकारी के लिए)
- [Ruby Date Documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby Time Documentation](https://ruby-doc.org/core-3.0.0/Time.html)
- [strftime Directives](https://apidock.com/ruby/DateTime/strftime)
