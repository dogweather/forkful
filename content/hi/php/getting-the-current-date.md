---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:16:30.765536-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
PHP में वर्तमान तारीख निकालना मतलब आज का दिन, महीना, साल जानना होता है। प्रोग्रामर्स यूजर के लिए टाइमलाइन्स बनाते समय, लॉग्स बनाने में, या समय-संवेदी डेटा की प्रोसेसिंग में इसका इस्तेमाल करते हैं।

## How to: (कैसे करें:)
```PHP
<?php
// वर्तमान तारीख को YYYY-MM-DD फॉर्मेट में दिखाने के लिए
echo date("Y-m-d") . "\n";

// समय के साथ वर्तमान तारीख - YYYY-MM-DD HH:MM:SS
echo date("Y-m-d H:i:s") . "\n";
?>
```
सैंपल आउटपुट:
```
2023-04-01
2023-04-01 15:42:08
```

## Deep Dive (गहराई से जानकारी):
PHP में `date()` फंक्शन का इस्तेमाल करके वर्तमान तारीख और समय निकालना काफी सीधा है। यह फंक्शन यूनिक्स एपोक (Unix Epoch - 1 जनवरी 1970) से शुरू होकर समय का हिसाब रखता है। इसमें ढेर सारे पैरामिटर्स होते हैं जिनसे तारीख और समय के विभिन्न फॉर्मेट्स को डिस्प्ले किया जा सकता है। 

फंक्शन `date_default_timezone_set()` का उपयोग करके आप वर्तमान टाइमज़ोन सेट कर सकते हैं और उसके अनुसार तारीख और समय प्राप्त कर सकते हैं। वैकल्पिक रूप से, `DateTime` क्लास और कार्बन जैसे थर्ड-पार्टी पैकेजेस भी तारीख और समय को हैंडल करने के लिए उपलब्ध हैं, जो और भी जटिल तरीकों को आसान बना सकते हैं।

## See Also (और भी देखें):
- [PHP date() Function](https://www.php.net/manual/en/function.date.php)
- [PHP DateTime Class](https://www.php.net/manual/en/class.datetime.php)
- [PHP.net Timezones](https://www.php.net/manual/en/timezones.php)
- [Carbon: A simple PHP API extension for DateTime](https://carbon.nesbot.com/)
