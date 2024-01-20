---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "PHP: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
भविष्य या अतीत में तारीख की गणना "डेट मेनिपुलेशन" है, जो किसी विशिष्ट समय अवधि के बाद या पहले की तारीख का पता लगाने की प्रक्रिया है। प्रोग्रामर्स को इसकी जरुरत पड़ सकती है क्‍यूंकि वे कस्टम आरेखों, समयधारित पाबंदियां ऐडवान्स प्लानिंग के लिए जांच सकते हैं।

## कैसे करें:
आपके आधार पर, PHP में दिनांक की गणना की उदाहरण::

```PHP
<?php
$date = new DateTime('2022-01-01');
$date->add(new DateInterval('P10D'));
echo $date->format('Y-m-d') . "\n";
?>
```

उत्पादन::

```PHP
2022-01-11
```

उपरोक्त कोड उदाहरण में, हमने सिर्फ 10 दिनों की वृद्धि की है।

## गहरी डाइव
दिनांक मेनिपुलेशन को PHP में काफी पुराने समय से समर्थित किया जा रहा है। कई वैकल्पिक तरीके मौजूद हैं, जैसे कि `strtotime()` और `DateTime::modify()`, परांतु `DateTime` और `DateInterval` आधिकारिक तौर पर सबसे ठोस और विश्वसनीय माने जाते हैं। 

डेट मेनिपुलेशन की कार्रवाई का परिणाम कांप्यूटर सिस्टम और समय क्षेत्र के चरित्र पर निर्भर कर सकता है, इसलिए इसे विचारना बहुत महत्वपूर्ण है।

## ये भी देखें
- [PHP अधिकारिक डॉक्यूमेंटेशन](https://www.php.net/manual/en/book.datetime.php)
- [PHP Datetime क्लास](https://www.php.net/manual/en/class.datetime.php)
- [PHP DateInterval क्लास](https://www.php.net/manual/en/class.dateinterval.php)