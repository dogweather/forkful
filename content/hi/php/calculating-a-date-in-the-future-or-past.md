---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:32:13.344120-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीख की गणना करना मतलब भूतकाल या भविष्यकाल में तारीखों का पता लगाना है। प्रोग्रामर्स इसे डेटा के विश्लेषण, एक्सपायरी तारीखों की जाँच और समय से जुड़ी गणनाएं करने के लिए करते हैं।

## How to: (कैसे करें:)

```PHP
<?php
// आज की तारीख
$today = new DateTime();

// भविष्य में 10 दिनों के लिए
$interval = new DateInterval('P10D');
$futureDate = (clone $today)->add($interval);
echo $futureDate->format('Y-m-d') . "\n"; // उदाहरण आउटपुट: 2023-04-21

// भूतकाल में 10 दिनों के लिए
$pastDate = (clone $today)->sub($interval);
echo $pastDate->format('Y-m-d') . "\n"; // उदाहरण आउटपुट: 2023-04-01
?>
```

## Deep Dive (गहन जानकारी):

तारीखों की गणना करने के लिए PHP में 'DateTime' क्लास और 'DateInterval' क्लास का इस्तेमाल होता है। `DateTime` 5.2.0 संस्करण में जोड़ा गया, और इससे तारीखों का प्रबंधन आसान हो गया। इससे पहले 'strtotime()' फंक्शन का उपयोग होता था, जो अब भी उपयोगी है लेकिन 'DateTime' अधिक लचीलापन और विश्वसनीयता प्रदान करता है।

`DateInterval` का उपयोग करके हम विशिष्ट समयावधियों को जोड़ने या घटाने के लिए कर सकते हैं। इसका 'P' प्रारूप ISO 8601 तिथि अंतर मानक का हिस्सा है, जिसमें 'P' का अर्थ है "period"। हम 'Y', 'M', 'D', 'H', 'M', और 'S' का इस्तेमाल करके साल, महीने, दिन, घंटे, मिनट और सेकंड को निर्देशित कर सकते हैं।

## See Also (और भी देखें):

- PHP Manual on DateTime: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- PHP Manual on DateInterval: [php.net/manual/en/class.dateinterval.php](https://www.php.net/manual/en/class.dateinterval.php)
- Date and time related functions in PHP: [php.net/manual/en/ref.datetime.php](https://www.php.net/manual/en/ref.datetime.php)
