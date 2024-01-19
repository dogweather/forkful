---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
दो तारीखों की तुलना करना, अर्थात एक की अपेक्षा दूसरी समय-बिंदु पर होनेवाली घटना को मुकाबला करना होता है। प्रोग्रामर्स इसे करते हैं ताकि उन्हें पास्ट, प्रेसेंट, और फ्यूचर घटनाओं के बीच का सही संबंध ज्ञान हो सके।

## कैसे करें:
यहां कुछ कोडिंग उदाहरण और उनका लिखित उत्तर दिया गया है:

```PHP
<?php
$date1 = new DateTime('2022-01-01');
$date2 = new DateTime('2023-01-01');

if ($date1 < $date2)
    echo "Date1 is earlier than Date2";
else
    echo "Date2 is earlier than Date1";
?>
```

ऊपरी प्रोग्राम का आउटपुट होगा: "Date1 is earlier than Date2" 

## गहरी दुबकी:
(1) ऐतिहासिक प्रसंग: PHP में एक नयी DateTime वर्ग ने कोडर्स को दिनांकों के साथ काम करने का एक सुविधाजनक तरीका प्रदान किया है।

(2) वैकल्पिक: PHP में strtotime() और date_diff() जैसे फ़ंक्शन भी हैं जिनका उपयोग करके दिनांकों की तुलना की जा सकती है।

(3) कार्यान्वयन विवरण: DateTime वर्ग के द्वारा मिलने वाली फंक्शनलिटी पास्ट, प्रेसेंट, और फ्यूचर डेटासेट्स को कार्यक्रमांकित करने के लिए काफी कारगर हैं।

## यह भी देखें:
1. PHP Manual (https://www.php.net/manual/en/book.datetime.php)
2. W3Schools PHP datetime functions (https://www.w3schools.com/php/php_ref_date.asp)