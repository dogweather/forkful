---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

पूंजीकरण का मतलब है एक स्ट्रिंग के अक्षरों को बड़ा (capital) करना। प्रोग्रामर इस तरह करते हैं ताकि डेटा को साफ-सुथरा और एकरूपता से प्रस्तुत कर सकें, जैसे कि पहला नाम या किसी शीर्षक को सुधारना।

## How to: (कैसे करें?)

PHP में स्ट्रिंग को कैपिटलाइज़ करना बहुत सरल है। नीचे दिए गए कोड में कुछ फ़ंक्शन के उदाहरण हैं:

```PHP
<?php
// सभी अक्षरों को ऊपरी मामले में बदलना
$upperCaseString = strtoupper("namaste duniya!");
echo $upperCaseString; // Output: NAMASTE DUNIYA!

// केवल पहले अक्षर को ऊपरी मामले में बदलना
$capitalizedString = ucfirst("namaste duniya!");
echo $capitalizedString; // Output: Namaste duniya!

// हर शब्द के पहले अक्षर को ऊपरी मामले में बदलना
$titleCaseString = ucwords("namaste duniya!");
echo $titleCaseString; // Output: Namaste Duniya!
?>
```

## Deep Dive (गहराई से समझें)

स्ट्रिंग को कैपिटलाइज़ करने की ज़रूरत तब देखी गई जब डेटा प्रदर्शन में एकरूपता महत्वपूर्ण हो गई। `strtoupper()`, `ucfirst()`, और `ucwords()` PHP के बुनियादी फ़ंक्शन हैं जो अलग-अलग केसिंग जरूरतों के लिए हैं। ये फ़ंक्शन मल्टीबाइट चरित्र से तालमेल नहीं रखते (जैसे यूनिकोड), ऐसे में `mb_strtoupper()`, `mb_convert_case()` आदि मल्टीबाइट फ़ंक्शन का इस्तेमाल कर सकते हैं। इसी तरह के कार्यों के लिए, जैसा कि स्ट्रिंग को कम मामले में बदलना, संबंधित फंक्शंस `strtolower()` और `mb_strtolower()` भी हैं।

## See Also (और देखें)

- PHP String Functions - [PHP.net Manual](https://www.php.net/manual/en/ref.strings.php)
- Multibyte String Functions - [PHP.net Manual](https://www.php.net/manual/en/ref.mbstring.php)
- PHP ucwords() Function - [W3Schools](https://www.w3schools.com/php/func_string_ucwords.asp)
