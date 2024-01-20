---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

स्ट्रिंग की लंबाई ढूंढना एक प्रोग्रामिंग कार्य होता है जिसमें हम यह जानते हैं कि किसी विशेष स्ट्रिंग में कितने वर्ण हैं। यह उन केस में उपयोगी होता है जब हमें कोड को अधिक प्रभावी और कम बग योग्य बनाने के लिए स्ट्रिंग के आकार को जानने की आवश्यकता होती है। 

## कैसे: 

PHP में, हम इस्तेमाल करके एक स्ट्रिंग की लंबाई जांच सकते हैं `strlen()` फ़ंक्शन। यहां एक उदाहरण है:

```php
<?php
$text = "नमस्ते, दुनिया!";
echo strlen($text);  
?>
```
उपरोक्त कोड स्निपेट का आउटपुट `14` होगा, क्योंकि इसमें 14 अक्षर हैं।

## गहरा डाइव:

स्ट्रिंग की लंबाई की जांच ने PHP के शुरुआती दिनों से ही हस्तियाँ ली है। `strlen()` फ़ंक्शन का उपयोग UTF-8 संगत टेक्स्ट के साथ नहीं किया जा सकता है, ऐसे मामलों में आपको `mb_strlen()` फ़ंक्शन का उपयोग करना होगा। यह एक बहुबाहासित विषय है, क्योंकि यहां यूनिकोड संगतता के मामले उत्पन्न हो सकते हैं। 

## देखें भी: 

1. PHP का आधिकारिक [strlen() प्रलेखन](https://www.php.net/manual/en/function.strlen.php)
2. [mb_strlen() प्रलेखन](https://www.php.net/manual/en/function.mb-strlen.php) के लिए PHP वेबसाइट पर जाएं.
3. Unicode संगतता के बारे में अधिक जानकारी के लिए, यह पढ़ें [PHP: Unicode संगतता कैसे काम करती है?](https://stackoverflow.com/questions/2109652/how-does-php-handle-unicode-and-what-is-the-best-practice-to-handle-unicode-in) पर StackOverflow.