---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:36.201103-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

तारीखों की तुलना का मतलब है दो अलग-अलग तारीखों को आपस में मिलाना। प्रोग्रामर्स यह जानने के लिए तारीखों की तुलना करते हैं कि कौन सी तारीख पहले है, कौन सी बाद में, या क्या वे समान हैं।

## How to: (कैसे करें:)

PHP में तारीखों की तुलना सीधे और आसान है। `DateTime` क्लास का इस्तेमाल करके देखो:

```PHP
<?php
// दो तारीख बनाओ
$date1 = new DateTime("2023-03-01");
$date2 = new DateTime("2023-04-01");

// तारीखों की तुलना करो
if ($date1 < $date2) {
    echo "date1 पहले की है।";
} elseif ($date1 == $date2) {
    echo "दोनों तारीखें समान हैं।";
} else {
    echo "date2 पहले की है।";
}
?>
```

इस कोड का आउटपुट होगा:

```
date1 पहले की है।
```
## Deep Dive (गहराई में जानकारी):

`DateTime` класс PHP 5.2 से आ रहा है, और यह तारीखों के साथ काम करने का एक उत्तम तरीका है। इससे पहले, `strtotime()` और `mktime()` जैसे फंक्शन्स का उपयोग होता था, लेकिन `DateTime` ज्यादा लचीला और ऑब्जेक्ट-ओरिएंटेड है।

आप `DateTime` द्वारा और भी तारीख के ऑपरेशन जैसे कि तारीख में जोड़ना-घटाना, टाइमज़ोन मैनेजमेंट, और फॉर्मेट बदलना, कर सकते हैं। तुलना के लिए, `DateTime::diff()` मेथड दो तारीखों के बीच के अंतर को `DateInterval` ऑब्जेक्ट के रूप में लौटाता है, जिसे आप और पढ़ सकते हैं। 

## See Also (और भी देखें):

- PHP ऑफिशल डॉक्यूमेंटेशन पर `DateTime` क्लास: https://www.php.net/manual/en/class.datetime.php
- `DateTime::diff()` के बारे में जानकारी: https://www.php.net/manual/en/datetime.diff.php
- PHP में टाइमज़ोन हैंडल करना: https://www.php.net/manual/en/timezones.php
