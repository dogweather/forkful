---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग से डेटा पार्स करना का मतलब है कि आप एक स्ट्रिंग से डेट के कॉम्पोनेंटों को निकालते हैं, जैसे कि दिन, महीना, वर्ष। यह तभी उपयोगी होता है जब आपको प्रोग्राम में भिन्न डेट कॉम्पोनेंट्स की जरूरत हो। 

## कैसे करें:

```PHP
<?php
$dateString = '2020-10-22';
$datetime = new DateTime($dateString);

echo $datetime->format('Y-m-d');
?>
```

ऊपर दिए गए कोड का आउटपुट इस प्रकार होता है:

```PHP
2020-10-22
```

## गहराई में:

PHP का 'DateTime' क्लास बहुत ही शक्तिशाली है और इसे PHP5 के संस्करण में एड किया गया था। मूलतः स्ट्रिंग्स से डेट वैल्यूज़ को पार्स करने के लिए 'strtotime()' फ़ंक्शन का उपयोग किया जाता था, लेकिन 'DateTime' क्लास इसे और भी आसान बनाती है।

अल्टरनेटिवली, तिथि पार्स करने के लिए 'date_parse()' फ़ंक्शन का भी इस्तेमाल किया जा सकता है। इसका उपयोग तब करना बेहतर होता है जब आपके पास एक विशिष्ट फ़ॉर्मेट का स्ट्रिंग नहीं हो। 

संगतता का प्रश्न उठ सकता है क्योंकि 'DateTime' क्लास PHP 5.2.0 और उसके बाद के संस्करणों में ही उपलब्ध है। 

## देखे भी:

- [PHP manual for DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP 'date_parse' function](https://www.php.net/manual/en/function.date-parse.php)
- [strtotime() function](https://www.php.net/manual/en/function.strtotime.php)