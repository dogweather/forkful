---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को लोअर केस में कन्वर्ट करना इसका अर्थ है कि हम सभी अक्षरों को छोटे अक्षरों में परिवर्तित कर रहे हैं। कई बार, डाटा संघटन और तुलना के दृष्टिकोण से, प्रोग्रामर्स इसे करने की आवश्यकता महसूस करते हैं। 

## कैसे करें:
PHP में, हम `strtolower()` फ़ंक्शन का उपयोग करके ऐसा कर सकते हैं। नीचे एक उदाहरण है:

```PHP
<?php 
$string = "Hello, World!";
$lowercaseString = strtolower($string);
echo $lowercaseString;
?>
```
आउटपुट:
```PHP
hello, world!
```
## गहराई में जाने:
समय के साथ, PHP कन्वर्शन की क्षमता को अग्रसर करने के लिए कई फ़ंक्शन्स को जोड़ा गया है। `strtolower()` PHP 4 और PHP 5 में उपलब्ध था, और इसकी कार्यक्षमता PHP 7 और PHP 8 में बना रही है। 

स्ट्रिंग को लोअर केस में कन्वर्ट करने के वैकल्पिक तरीके हैं, जैसे `mb_strtolower()` जो मल्टीबाइट स्ट्रिंग को लोअरकेस में करता है। 

`strtolower()` फ़ंक्शन PHP के C भाषा एक्सटेंशन के आधार पर बनाया गया है जिसमें स्ट्रिंग ट्रांसफ़ॉर्मेशन के लिए नीचे की ओर कोड जोड़ा गया है।

## अधिक जानकारी के लिए:
1. PHP `strtolower()` फ़ंक्शन: https://www.php.net/manual/en/function.strtolower.php
2. PHP `mb_strtolower()` फ़ंक्शन: https://www.php.net/manual/en/function.mb-strtolower.php
3. विपरीत, स्ट्रिंग को कैसे अपरकेस में बदलें: https://www.php.net/manual/en/function.strtoupper.php