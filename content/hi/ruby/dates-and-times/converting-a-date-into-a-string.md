---
date: 2024-01-20 17:37:49.354835-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Ruby \u092E\
  \u0947\u0902 \u0921\u0947\u091F \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\
  \u0902\u0917 \u092E\u0947\u0902 \u092A\u0930\u093F\u0935\u0930\u094D\u0924\u093F\
  \u0924 \u0915\u0930\u0928\u093E `Date` \u0914\u0930 `Time` \u0915\u094D\u0932\u093E\
  \u0938\u0947\u0938 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u0915\u093F\u092F\u093E \u091C\u093E\
  \u0924\u093E \u0939\u0948\u0964 `strftime` \u092E\u0947\u0925\u0921 \u0938\u092C\
  \u0938\u0947 \u0909\u092A\u092F\u094B\u0917\u0940\u2026"
lastmod: '2024-04-05T22:51:07.915714-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Ruby \u092E\u0947\u0902\
  \ \u0921\u0947\u091F \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u092E\u0947\u0902 \u092A\u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\
  \u0930\u0928\u093E `Date` \u0914\u0930 `Time` \u0915\u094D\u0932\u093E\u0938\u0947\
  \u0938 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \u0924\u0947 \u0939\u0941\u090F \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\
  \ \u0939\u0948\u0964 `strftime` \u092E\u0947\u0925\u0921 \u0938\u092C\u0938\u0947\
  \ \u0909\u092A\u092F\u094B\u0917\u0940 \u0939\u0948, \u091C\u094B \u0935\u093F\u092D\
  \u093F\u0928\u094D\u0928 \u092A\u094D\u0930\u0915\u093E\u0930 \u0915\u0947 \u092B\
  \u0949\u0930\u094D\u092E\u0947\u091F\u093F\u0902\u0917 \u0935\u093F\u0915\u0932\u094D\
  \u092A \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\
  \u0964 \u092F\u0939 UNIX \u0938\u093F\u0938\u094D\u091F\u0947\u092E \u092E\u0947\
  \u0902 \u0926\u0947\u0916\u0947 \u0917\u090F `strftime` \u092B\u0902\u0915\u094D\
  \u0936\u0928 \u0938\u0947 \u092A\u094D\u0930\u0947\u0930\u093F\u0924 \u0939\u0948\
  \u0964 \u0935\u093F\u0915\u0932\u094D\u092A \u0915\u0947 \u0930\u0942\u092A \u092E\
  \u0947\u0902, `to_s` \u092E\u0947\u0925\u0921 ISO 8601 \u092B\u0949\u0930\u094D\u092E\
  \u0947\u091F \u092E\u0947\u0902 \u0921\u0947\u091F \u0915\u094B \u0921\u093F\u092B\
  \u0949\u0932\u094D\u091F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092B\
  \u0949\u0930\u094D\u092E\u0947\u091F \u092E\u0947\u0902 \u092C\u0926\u0932\u0924\
  \u093E \u0939\u0948\u0964 \u0905\u0917\u0930 \u0915\u0938\u094D\u091F\u092E\u093E\
  \u0908\u091C\u094D\u0921 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092B\
  \u0949\u0930\u094D\u092E\u0947\u091F \u091A\u093E\u0939\u093F\u090F \u0939\u094B\
  \ \u0924\u094B, `strftime` \u0915\u0947 \u0915\u0908 \u0921\u093E\u092F\u0930\u0947\
  \u0915\u094D\u091F\u093F\u0935\u094D\u0938 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  , \u091C\u0948\u0938\u0947 \u0915\u0940 `%d` \u0926\u093F\u0928 \u0915\u0947 \u0932\
  \u093F\u090F, `%m` \u092E\u0939\u0940\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0914\u0930 `%Y` \u0938\u093E\u0932 \u0915\u0947 \u0932\u093F\u090F\u0964."
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
