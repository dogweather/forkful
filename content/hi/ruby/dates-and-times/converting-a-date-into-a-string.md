---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases: - /hi/ruby/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:49.354835-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग में परिवर्तित करने का मतलब है तारीख को टेक्स्ट फॉर्म में बदलना। प्रोग्रामर्स ये इसलिए करते हैं क्योंकि कई बार डेटा को स्टोर करने, लॉग करने या यूजर इंटरफेस पर दिखाने के लिए आसानी होती है।

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
