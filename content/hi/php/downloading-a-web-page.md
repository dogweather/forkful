---
title:                "एक वेब पेज को डाउनलोड करना"
html_title:           "PHP: एक वेब पेज को डाउनलोड करना"
simple_title:         "एक वेब पेज को डाउनलोड करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वेब पृष्ठ डाउनलोड करने का मुख्य कारण है कि उपयोगकर्ता दूसरों को उस पृष्ठ के साथ आसानी से अपना अनुभव साझा कर सकते हैं। यह भी उपयोगकर्ताओं को अनुकूल डेटा को सुरक्षित रखने में मदद करता है।

## कैसे करें

```
<?php
$url = "https://www.example.com";
$file = file_get_contents($url);

echo $file;
?>
```

इस उदाहरण में, हमने `file_get_contents` फ़ंक्शन का उपयोग करके दिए गए URL से पृष्ठ को डाउनलोड किया है और उसे `echo` के माध्यम से प्रिंट किया है। आप अपनी जरूरत के अनुसार इसे अपनाकर और भी विस्तृत डाउनलोड प्रक्रिया बना सकते हैं।

## गहराई में जाएं

URL से पृष्ठ डाउनलोड करने के लिए, कभी-कभी कई प्रकार के डेटा भी आपको चाहिए होते हैं, जो उपयोगकर्ताओं को उस पृष्ठ के संबंध में अधिक जानकारी प्रदान करते हैं। आपको [cURL](https://www.php.net/manual/en/book.curl.php) जैसे और भी अलग-अलग विकल्प मिलेंगे, जिन्हें आप अपनी आवश्यकता के अनुसार उपयोग कर सकते हैं। साथ ही, आप सरल और सुरक्षित अनुरोध पद्धति के साथ विकेंध वेब विभिन्न वेब होस्टिंग प्लेटफ़ॉर्म्स पर डाउनलोड प्रोसेस को प्रकाशित कर सकते हैं।

## इससे सम्बंधित

- [PHP file_get_contents() Function](https://www.php.net/manual/en/function.file-get-contents.php)
- [An Introduction to cURL in PHP](https://www.php.net/manual/en/book.curl.php)
- [cURL Tutorial: How to use cURL and PHP](https://www.phpied.com/curl/)
- [PHP cURL Tutorial: How to Create GET, POST, DELETE Requests](https://www.codeofaninja.com/2013/05/php-curl-tutorial.html)