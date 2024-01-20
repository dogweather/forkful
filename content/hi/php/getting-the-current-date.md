---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्तमान तिथि प्राप्त करना यानी किसी कंप्यूटर सिस्टम के अनुसार "अब" कैलेंडर दिन की जानकारी प्राप्त करना। कार्यक्रमकर्ताओं को वर्तमान दिनांक की आवश्यकता होती है क्योंकि यह डाटा लॉगिंग, टाइमस्टैंपस या इत्यादि में सहायक होता है।

## किस प्रकार:

PHP में वर्तमान दिनांक प्राप्त करने के लिए, हम `date` function का उपयोग करते हैं:

```PHP
<?php
echo date('Y-m-d H:i:s');
?>
```

आउटपुट कुछ इस तरह होगा:

```
2022-03-10 14:50:47
```

## गहराई में:

PHP में की तारीख को प्राप्त करने वाले कोड का विकास 1994 में PHP की शुरुआत के साथ ही हुआ। बदलावों के साथ, वर्तमान `date` function सटीकता और ऑपरेटर अवगति को बढ़ाने के लिए डिज़ाइन किया गया है।

विकल्पों में, कर्मचारी हमेशा वर्तमान समय को Unix टाइमस्टैंप (`time` function का उपयोग करके) के रूप में प्राप्त कर सकते हैं:

```PHP
<?php
echo time();
?>
```

आपके पास विशेष "DateTime" वर्ग का विकल्प भी होता है, जो अधिक परिपूर्ण तारीख/समय कार्यकारिता प्रदान करता है:

```PHP
<?php
$date = new DateTime();
echo $date->format('Y-m-d H:i:s');
?>
```

## देखने के लिए:

आगे की पढ़ाई के लिए, निम्नलिखित संसाधनों की जांच करें:

1. PHP का औपचारिक प्रलेखन: [https://www.php.net/manual/en/function.date.php](https://www.php.net/manual/en/function.date.php)
2. PHP DateTime Class Documentation: [https://www.php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)