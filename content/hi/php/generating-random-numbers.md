---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों?

यादृच्छिक संख्याएं उत्पन्न करना वह प्रक्रिया होती है जो प्रोग्रामरों द्वारा अदृश्य संख्याओं को जनमानस में लाने के लिए की जाती है। इसका प्रमुख उपयोग यादृच्छिकता उत्पन्न करने, एन्क्रिप्शन तथा कई अन्य कामों में किया जाता है। 

## कैसे करें:

```PHP
<?php
// यादृच्छिक संख्या उत्पादित करें
$randomNumber = rand();

echo $randomNumber;
?>
```
ऊपरे दिए गए कोड में, `rand()` फंक्शन एक यादृच्छिक संख्या उत्पन्न करेगा जिसकी विस्तृति आप स्वतंत्रता से निर्धारित कर सकते हैं।

## गहरी डाइव:

(1) ऐतिहासिक संदर्भ: 
PHP में यादृच्छिक संख्याएँ उत्पन्न करने के लिए `rand()` फ़ंक्शन का प्रयोग किया जाता था, लेकिन बाद में `mt_rand()` और `random_int()` जैसे अधिक बेहतर विकल्पों का विकास हुआ।

(2) विकल्प: 
गणनात्मक कामों के लिए `rand()` का प्रयोग कर सकते हैं, लेकिन क्रिप्टोग्राफीय उपयोगों के लिए `random_int()` अथवा `openssl_random_pseudo_bytes()` जैसी फ़ंक्शन का उपयोग करें। 

(3) कार्यान्वयन विवरण: 
PHP में यादृच्छिक संख्या उत्पन्न करने के लिए उपयोग की जाने वाली फ़ंक्शन, अपने आंतरिक तंत्र पर आधारित होती है और उनका आउटपुट प्लेटफ़ॉर्म तथा भाषा संस्करण के आधार पर बदल सकता है। 

## देखें भी:

1. [PHP Manual: rand()](https://www.php.net/manual/en/function.rand.php)
2. [PHP Manual: mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
3. [PHP Manual: random_int()](https://www.php.net/manual/en/function.random-int.php)
4. [PHP Manual: openssl_random_pseudo_bytes()](https://www.php.net/manual/en/function.openssl-random-pseudo-bytes.php)