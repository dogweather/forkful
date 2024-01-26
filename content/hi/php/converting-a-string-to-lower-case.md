---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:39:17.560863-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में कन्वर्ट करने का मतलब है कि हर अक्षर को छोटे अक्षर में बदल देना। प्रोग्रामर्स ऐसा इसलिए करते हैं ताकि वो केस सेंसिटिविटी को नेविगेट कर सकें और डेटा को साफ-सुथरा और स्टैंडर्डाइज़ कर सकें।

## How to (कैसे करें):
PHP में स्ट्रिंग को लोअर केस में कैसे बदलें, यहाँ उदाहरण दिया गया है:

```php
<?php
$originalString = "Namaste Duniya!";
$lowerCaseString = strtolower($originalString);
echo $lowerCaseString; // "namaste duniya!"
?>
```

## Deep Dive (गहराई से जानकारी):
इतिहास के संदर्भ में, `strtolower()` फंक्शन PHP के शुरुवाती वर्जन से ही मौजूद है। यह UTF-8 एन्कोडेड स्ट्रिंग्स के लिए `mb_strtolower()` जैसे फंक्शन के साथ आया, जो बहुभाषी आवश्यकताओं के लिए जरूरी है। इसका प्रयोग आपस में तुलना करने, सर्च और सॉर्टिंग के दौरान डेटा की एकरूपता बनाने के लिए होता है। `mb_strtolower` फंक्शन अधिक व्यापक चरित्र सेट सपोर्ट के साथ आता है और यह सिफारिश की जाती है जब आप अंतरराष्ट्रीय करैक्टर सेट्स के साथ काम कर रहे हों।

## See Also (और भी जानकारी):
- PHP ऑफिशियल डॉक्युमेंटेशन (strtolower): https://www.php.net/manual/en/function.strtolower.php
- PHP ऑफिशियल डॉक्युमेंटेशन (mb_strtolower): https://www.php.net/manual/en/function.mb-strtolower.php
- PHP मल्टीबाईट स्ट्रिंग फंक्शन के बारे में अधिक जानकारी: https://www.php.net/manual/en/ref.mbstring.php
