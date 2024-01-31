---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:16:54.623847-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या & क्यों? (What & Why?)
Ruby में वर्तमान दिनांक प्राप्त करना कोड में आज की तारीख देखने जैसा है। प्रोग्रामर लॉगिंग, वेबसाइट्स पर टाइमस्टैम्प, और डेटा के विश्लेषण के लिए इसे उपयोग करते हैं।

## कैसे करें? (How to:)
Ruby में वर्तमान दिनांक प्राप्त करें:

```Ruby
require 'date'

# वर्तमान दिनांक और समय प्राप्त करना
current_date_time = DateTime.now
puts current_date_time

# केवल दिनांक प्राप्त करना
current_date = Date.today
puts current_date
```

सैंपल आउटपुट:
```
2023-04-12T15:23:45+05:30
2023-04-12
```

## गहराई से समझ (Deep Dive)
Ruby का 'DateTime' और 'Date' मॉड्यूल प्रोग्रामर्स को तारीख और समय के साथ काम करने की अनुमति देता है। 'DateTime.now' से आपको दिनांक के साथ समय भी मिल जाता है, जबकि 'Date.today' सिर्फ वर्तमान दिनांक देता है। मूल रूप से ये मॉड्यूल्स Ruby के मानक पुस्तकालय का हिस्सा हैं।

विकल्प के तौर पर, 'Time' क्लास भी है, जो 'Time.now' के माध्यम से वर्तमान समय को वापस करता है:

```Ruby
# वर्तमान समय प्राप्त करना
current_time = Time.now
puts current_time
```

समय और दिनांक को हेरफेर करने के लिए कई दूसरे रत्न (gems) भी उपलब्ध हैं, जैसे कि 'active_support' (Rails में इस्तेमाल)।

## यह भी देखें (See Also)
- Ruby के डॉक्स में Time: [Ruby-Doc.org Time](https://ruby-doc.org/core/Time.html)
