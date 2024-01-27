---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन्स (Regular Expressions) पाठ में पैटर्न सर्चिंग की तकनीक है। प्रोग्रामर्स इसका प्रयोग डाटा वैलिडेशन, खोज, और टेक्स्ट अपरिवर्तन के लिए करते हैं। 

## How to: (कैसे करें:)
```PHP
<?php
// मैचिंग ईमेल पैटर्न
$pattern = "/^([a-zA-Z0-9_\.-]+)@([a-zA-Z0-9-]+)\.([a-zA-Z]{2,6})$/";
$email = "example@mail.com";

if (preg_match($pattern, $email)) {
    echo "वैध ईमेल पता।";
} else {
    echo "अवैध ईमेल पता।";
}
// Output: वैध ईमेल पता।
?>
```

```PHP
<?php
// पाठ में फोन नंबरों का खोज
$text = "मेरा नंबर 123-456-7890 पर कॉल करें।";
$pattern = "/\b\d{3}[-.]?\d{3}[-.]?\d{4}\b/";

preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Output: Array ( [0] => 123-456-7890 )
?>
```

## Deep Dive (गहराई से जानकारी)
रेगुलर एक्सप्रेशन्स का इतिहास 1950 के दशक तक जाता है। अल्टरनेटिव के रूप में स्ट्रिंग फ़ंक्शन्स जैसे strpos(), strstr(), etc., उपलब्ध हैं पर उतनी सक्षम नहीं होतीं। PHP में पीसीआरई (PCRE - Perl Compatible Regular Expressions) लाइब्रेरी का प्रयोग रेगुलर एक्सप्रेशन्स के लिए किया जाता है।

## See Also (इसे भी देखें)
- PHP Manual on RegEx: [PHP: PCRE - Manual](https://www.php.net/manual/en/book.pcre.php)
- RegExr: [RegExr: Learn, Build, & Test RegEx](https://regexr.com/)
- RegEx101: [RegEx 101: Online regex tester and debugger](https://regex101.com/)
