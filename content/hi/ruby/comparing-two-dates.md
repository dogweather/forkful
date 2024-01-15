---
title:                "दो तारीखों की तुलना करना"
html_title:           "Ruby: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## आमतौर पर आपने दो तारीखों की तुलना क्यों की जाएगी?

तिथियों की तुलना करने का मुख्य कारण है कि आप दो तारीखों को एक ही स्थान पर देखकर उनमें संबंध देख सकते हैं जैसे कि कब एक तारीख दूसरी के बाद आती है। यह आपको समय और कठिनाई से बचाता है और कूदेंप्रतिस्पर्धिता को मजबूत बनाता है। 

## कैसे तारीखों की तुलना करे

```Ruby
date1 = Date.new(2020, 12, 25)
date2 = Date.new(2021, 1, 1)

# एक साल के बीच की दो तारीखों को तुलना करने के लिए
date2 > date1
=> true

# एक सप्ताह के बीच की दो तारीखों को तुलना करने के लिए
date2 - date1
=> 7
```

जैसा कि आप देख सकते हैं, हम साल और सप्ताह के बीच तारीखों की तुलना करते हैं और उससे हमें तारीखों के बीच कितने दिन का अंतर है पता चलता है। आप अन्य मापदंड भी उपयोग कर सकते हैं जैसे कि घंटों, मिनटों, दूरी आदि। 

## गहराई में जाइए 

तारीखों को तुलना करने में आगे बढ़ने के लिए, आपको थोड़ा गहराई से जाना होगा। तुलना करने के लिए मुख्य कारक हैं: सही मान्यता (validation) और समय की तुलना आपकी ज़रूरतों के अनुसार होनी चाहिए। आपको ध्यान रखना चाहिए कि दो तारीखों की तुलना करते समय ये उपयोगी हो सकते हैं: Date, Time, DateTime, ActiveSupport::Duration, और ActiveSupport::Duration के साथ Date, Time या DateTime।

## देखें भी 

- [Active Support Core Extensions](https://guides.rubyonrails.org/active_support_core_extensions.html)
- [Ruby Date और Time डॉक्यू