---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Elixir: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या है और क्यों करे?
एलिक्सिर में एक तारीख को स्ट्रिंग में बदलना ऐसे स्थापना को कहते हैं जो दिनांक को पढ़ने और समझने में मदद करता है। कई बार हम डेटा को स्टोर करते हैं और उसे प्रदर्शित करने के लिए स्ट्रिंग में बदलने की आवश्यकता होती है।

## कैसे करे:
```Elixir
date = Date.utc_today()
IO.puts(date) # 2021-08-21
date_string = NaiveDateTime.to_iso8601(date)
IO.puts(date_string) # 2021-08-21T00:00:00Z
```

## गहराई में जाएं:
तारीख को स्ट्रिंग में बदलने का अध्ययन बहुत ही महत्वपूर्ण है क्योंकि आजकल हमें अपने डेटा को संभालने और समझने के लिए अधिक और अधिक सुविधाएं चाहिए हैं। कई बार हम तारीख को स्ट्रिंग में बदलने को देख रहे हैं तो ये भी देख सकते हैं कि कौन से अल्टरनेटिव हमें उपलब्ध हैं और द्वारा कैसे एलिक्सिर डेटा को स्ट्रिंग में बदलता है।

## देखें:
अधिक जानकारी के लिए आप निम्नलिखित स्रोतों की जाँच कर सकते हैं:
- [एलिक्सिर आधिकारिक डॉक्युमेंटेशन](https://elixir-lang.org/getting-started/basic-types.html#date-time-and-time)
- [प्रोग्रामिंग में तारीख और समय को कैसे आवश्यक किया जाएं](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-elixir)
- [एलिक्सिर में तारीख को स्ट्रिंग में बदलने की प्रक्रिया](https://cultivatehq.com/posts/date-string-formats-in-elixir/)