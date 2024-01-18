---
title:                "स्ट्रिंग से तारीख को अलग करना"
html_title:           "PHP: स्ट्रिंग से तारीख को अलग करना"
simple_title:         "स्ट्रिंग से तारीख को अलग करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
पाठ से तारीख को पारस्कृत करना क्या है और प्रोग्रामर इसे क्यों करते हैं, यह दो तीन वाक्यों में समझाया गया है।

## कैसे: 
फिर से स्ट्रिंग की तारीख को गणना करना आपको PHP में जानकारी नापने के लिए कुछ कोड का उपयोग करने की आवश्यकता होती है। इसके लिए निम्नलिखित उदाहरणों का पालन करें और उदाहरण आउटपुट देखें।

```PHP
// टेस्ट स्ट्रिंग की तारीख को पारस्कृत करें
$date = date_create_from_format('d/m/Y', '25/12/2019');
// तारीख को डेटा फॉर्मेट में प्रिंट करें
echo date_format($date, 'Y-m-d');
// तारीख को मास के नाम से प्रिंट करें
echo date_format($date, 'F d, Y');
// तारीख को दिनों को जोड़कर प्रिंट करें
echo date_format($date, 'd + 3');
```

आउटपुट:
```HTML
2019-12-25
December 25, 2019
28
```

## गहराई में जाएं: 
फिर से स्ट्रिंग से तारीख पारस्कृत करने के लिए, PHP ५ से समय के साथ में स्पं, डेटीटाईम, संख्या का अपडेट प्रद्रशन résister एंड तारीख को manipulate करने के लिए उपयोग में लाना। इसके लिए अल्ट्डरनेटिव्स के साथ समय और तारीख को क्राफ्त और diff मेथड का उपयोग किया गया है, जो आपको तारीख से संबंधित जानकारी प्रकट करता है।

## सी भी देखें: 
स्ट्रिंग से तारीख को पारस्कृत करने के बारे में और जानने के लिए, आप निम्न स्रोतों की जांच कर सकते हैं:

- [PHP.net](https://www.php.net/manual/en/function.date-create-from-format.php)
- [W3schools](https://www.w3schools.com/php/func_date_date_create_from_format.asp)
- [Geekforgeeks](https://www.geeksforgeeks.org/how-to-extract-the-time-string-from-the-date-string-using-php/)