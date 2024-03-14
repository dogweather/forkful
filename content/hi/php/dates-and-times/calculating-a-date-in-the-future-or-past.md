---
date: 2024-01-20 17:32:13.344120-07:00
description: "\u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\u093E\
  \ \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u092D\u0942\u0924\u0915\u093E\
  \u0932 \u092F\u093E \u092D\u0935\u093F\u0937\u094D\u092F\u0915\u093E\u0932 \u092E\
  \u0947\u0902 \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u093E \u092A\u0924\
  \u093E \u0932\u0917\u093E\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u0947\
  \u091F\u093E \u0915\u0947 \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923, \u090F\
  \u0915\u094D\u0938\u092A\u093E\u092F\u0930\u0940 \u0924\u093E\u0930\u0940\u0916\u094B\
  \u0902 \u0915\u0940 \u091C\u093E\u0901\u091A \u0914\u0930 \u0938\u092E\u092F \u0938\
  \u0947 \u091C\u0941\u095C\u0940\u2026"
lastmod: '2024-03-13T22:44:52.501541-06:00'
model: gpt-4-1106-preview
summary: "\u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\u093E \u0915\
  \u0930\u0928\u093E \u092E\u0924\u0932\u092C \u092D\u0942\u0924\u0915\u093E\u0932\
  \ \u092F\u093E \u092D\u0935\u093F\u0937\u094D\u092F\u0915\u093E\u0932 \u092E\u0947\
  \u0902 \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u093E \u092A\u0924\u093E\
  \ \u0932\u0917\u093E\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u0947\u091F\
  \u093E \u0915\u0947 \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923, \u090F\u0915\
  \u094D\u0938\u092A\u093E\u092F\u0930\u0940 \u0924\u093E\u0930\u0940\u0916\u094B\u0902\
  \ \u0915\u0940 \u091C\u093E\u0901\u091A \u0914\u0930 \u0938\u092E\u092F \u0938\u0947\
  \ \u091C\u0941\u095C\u0940\u2026"
title: "\u092D\u0935\u093F\u0937\u094D\u092F \u092F\u093E \u0905\u0924\u0940\u0924\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u0940 \u0917\u0923\u0928\
  \u093E"
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
