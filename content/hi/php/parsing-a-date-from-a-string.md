---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:38:44.147376-07:00
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग से पार्स करना यानी स्ट्रिंग में दी गई डेट की जानकारी को निकालना, जैसे दिन, महीना, और साल। प्रोग्रामर इसलिए डेट पार्सिंग करते हैं क्योंकि डेटा संसाधित करने, वैध करने, या डेटेबेस में डेट को स्टोर करने के लिए स्टैंडर्ड फॉर्मेट में होना जरूरी होता है।

## How to: (कैसे करें:)
PHP में डेट पार्स करने के लिए `DateTime` क्लास और `date_create_from_format` फंक्शन का उपयोग करें।

```PHP
<?php
$dateString = "25-03-2023";
$dateObject = date_create_from_format('d-m-Y', $dateString);

// डेट ऑब्जेक्ट को चेक करें और फॉर्मेटेड स्ट्रिंग प्रिंट करें
if ($dateObject) {
    echo $dateObject->format('Y-m-d'); // आउटपुट: 2023-03-25
} else {
    echo "डेट वैध नहीं है।";
}

?>
```
यह कोड स्निपेट एक स्ट्रिंग को `DateTime` ऑब्जेक्ट में कन्वर्ट करता है और फिर उसे ISO 8601 फॉर्मेट में प्रिंट करता है।

## Deep Dive (गहन जानकारी)
पहले के वर्जन में PHP में डेट पार्स करने के लिए `strtotime()` और `date()` फंक्शन्स का इस्तेमाल होता था। अब, `DateTime` क्लास बेहतर टाइमज़ोन सपोर्ट और ऑब्जेक्ट ओरिएंटेड एप्रोच की वजह से ज़्यादा तरजीह पाता है। `date_create_from_format()` से आप कस्टम पैटर्न वाले डेट पार्स कर सकते हैं। अगर पार्सिंग में कोई गलती हो तो `DateTime::getLastErrors()` यूज कर के एरर्स को देख सकते हैं। विकल्प के रूप में, `strtotime`, `date_parse` और `strptime` जैसे फंक्शन्स भी हैं, पर वे उतने लचीले नहीं हैं और सभी फॉर्मेट्स को सपोर्ट नहीं करते।

## See Also (और जानें)
- [PHP Manual on DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP Manual on date_create_from_format](https://www.php.net/manual/en/function.date-create-from-format.php)
- [PHP Date/Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP The Right Way: Date and Time](https://phptherightway.com/#date_and_time)
