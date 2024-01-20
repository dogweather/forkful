---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "PHP: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को कैपिटलाइज करना मतलब होता है उसके हर शब्द के पहले अक्षर को बड़ा कर देना। कार्यक्रमकर्ता इसे स्पष्ट करने के लिए करते हैं कि कौन से शब्द महत्वपूर्ण हैं, जैसे कि टाइटल या मेन्यू विकल्प।

## कैसे करें:

PHP में, हम इसे `ucwords` फ़ंक्शन का उपयोग करके कर सकते हैं:

```PHP
<?php
$string = "नमस्ते दुनिया!";
$capitalizedString = ucwords($string);
echo $capitalizedString;
?>
```

इस कोड का आउटपुट होगा:

```
"नमस्ते दुनिया!"
```

(नोट: यह सब लिपियों पर काम करता है, न कि हिंदी पर ही।)

## गहरी जानकारी

`ucwords` फ़ंक्शन का इस्तेमाल PHP 4 से शुरू हुआ था। इसके विकल्प स्वरूप, PHP 5.4.32 और 5.5.16 में `mb_convert_case` फ़ंक्शन को जोड़ा गया, जिसने विभिन्न बायट एन्कोडिंग समर्थन का विकल्प दिया। `ucwords` फ़ंक्शन केवल शब्दों के पहले अक्षर को केपिटलाइज करता है, जबकि `mb_convert_case` फ़ंक्शन विभिन्न मामलों के साथ काम कर सकता है, जैसे `MB_CASE_UPPER`, `MB_CASE_LOWER`, और `MB_CASE_TITLE`।

## अन्य स्रोत देखें

अधिक जानकारी के लिए, आप PHP की आधिकारिक डॉक्युमेंटेशन की जांच कर सकते हैं:

1. [`ucwords` function documentation](https://www.php.net/manual/en/function.ucwords.php)
2. [`mb_convert_case` function documentation](https://www.php.net/manual/en/function.mb-convert-case.php)
3. [String functions in PHP](https://www.php.net/manual/en/ref.strings.php)