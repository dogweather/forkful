---
title:                "तारीख को स्ट्रिंग में बदलना"
aliases: - /hi/php/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:39.180148-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
PHP में तारीख को स्ट्रिंग में बदलने का मतलब है तारीख का डेटा फॉरमैट को पाठ रूप में प्रस्तुत करना। प्रोग्रामर्स अक्सर इसे करते हैं ताकि वेब पेज पर दिखाने के लिए या डेटाबेस में स्टोर करने के लिए तारीख को सही फॉर्मेट में पेश किया जा सके।

## How to: (कैसे करें:)
```PHP
<?php
$originalDate = new DateTime('2023-04-01 14:35:00');
$formattedDate = $originalDate->format('d-m-Y H:i:s');
echo $formattedDate; // 01-04-2023 14:35:00
?>
```
यहां `DateTime` ऑब्जेक्ट का इस्तेमाल किया गया है और `format` मेथड से तारीख को स्ट्रिंग में बदला गया है। 

## Deep Dive (गहराई से जानकारी)
PHP में तारीखों को स्ट्रिंग्स में बदलने का चलन शुरुआती वर्ज़न्स से चला आ रहा है। शुरू में `date()` फंक्शन ज्यादा इस्तेमाल होता था, लेकिन अब `DateTime` क्लास अधिक लचीलापन और बेहतर ऑब्जेक्ट-ओरिएंटेड तरीके प्रदान करती है। अलग-अलग फॉरमैट का इस्तेमाल करके हम लगभग किसी भी तरह की तारीख को स्ट्रिंग में बदल सकते हैं। `DateTime::format()` के अलग-अलग पैरामीटर अलग तरह की स्ट्रिंग फॉरमैट प्रदान करते हैं, जबकि `DateTimeZone` का उपयोग समय क्षेत्र के अनुसार तारीख फॉरमैट करने के लिए किया जाता है।

## See Also (और भी देखें)
- PHP का आधिकारिक डॉक्यूमेंटेशन `DateTime` पर: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- `date()` फ़ंक्शन पर PHP मैन्युअल: [php.net/manual/en/function.date.php](https://www.php.net/manual/en/function.date.php)

नोट: 'See Also' सेक्शन के लिंक्स जो दिए गए हैं, उनमें आपको अपनी ज़रूरत के अनुसार वास्तविक और सही लिंक्स डालने होंगे।
