---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

दिनांक को विभिन्न प्रारूपों में परिवर्तित करने का काम PHP के द्वारा किया जाता है। कार्यक्रमकर्ताओं को दिनांक को विभाज्य (एक चर स्ट्रिंग) में परिवर्तित करने की आवश्यकता होती है। वे यह तारीख और समय के डेटा का प्रसंस्करण कарने और विभिन्न प्रारूपों में छापने के लिए इसे करते हैं।

## कैसे करें: 

PHP में दिनांक का तार स्ट्रिंग में परिवर्तन करने के लिए आप `date()` फ़ंक्शन का उपयोग कर सकते हैं। 

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s');
?>
```

ऊपरी कोड भाग में, हम एक नई DateTime ऑब्जेक्ट बनाते हैं और फिर ऐसा प्रारूप चुनते हैं जो हमें चाहिए।

## डीप डाइव: 

PHP में दिनांक को विभाज्य में परिवर्तन करने का विचार संवाददाता अनुप्रयोगों के लिए महत्वपूर्ण है, जैसे कि डेटाबेस के लिए सहेजने और विभिन्न समय क्षेत्रों के बीच में परिवर्तन। 

विकल्प:

1. `intval()` फ़ंक्शन: इसका उपयोग इंटीजर में स्ट्रिंग को परिवर्तित करने के लिए किया जा सकता है। 
2. `strtotime()`: इसका उपयोग स्ट्रिंग को Unix timestamp में परिवर्तित करने के लिए किया जा सकता है।

## और भी देखें: 

1. PHP डॉक्स - डेट और समय: [https://www.php.net/manual/en/book.datetime.php](https://www.php.net/manual/en/book.datetime.php)
2. PHP - डेटटाइम:: फॉर्मेट: [https://www.php.net/manual/en/datetime.format.php](https://www.php.net/manual/en/datetime.format.php)