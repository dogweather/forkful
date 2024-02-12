---
title:                "सबस्ट्रिंग्स निकालना"
aliases:
- /hi/php/extracting-substrings/
date:                  2024-01-20T17:46:32.158676-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
उप-स्ट्रिंग निकालना, यानी किसी स्ट्रिंग के एक भाग को अलग करना। यह तब जरूरी है जब किसी विशेष जानकारी की जरूरत हो - जैसे यूजर नेम, सर्च टर्म्स, या कोई विशिष्ट पैटर्न।

## How to: (कैसे करें:)

PHP में स्ट्रिंग्स से उप-स्ट्रिंग निकालने के लिए `substr` फंक्शन एक सरल और प्रचलित तरीका है। 

```PHP
<?php
$originalString = "नमस्ते दुनिया!";
$substring = substr($originalString, 7, 12);
echo $substring; // दुनिया!
?>
```

इस उदाहरण में, `$originalString` से `नमस्ते` के बाद का भाग, `दुनिया!`, निकाला गया है।

## Deep Dive (गहन अध्ययन):

'70 के दशक से, C प्रोग्रामिंग भाषा में `substr` जैसे फंक्शंस उपयोग में थे, और PHP ने भी इसको अपनाया। Alternatives में `mb_substr` है जो multibyte characters, जैसे हिंदी अक्षरों, के लिए उपयोगी है। `str_split` और `preg_split` जैसे फंक्शन्स भी हैं, लेकिन वे अलग काम के लिए होते हैं - पूरी स्ट्रिंग को टुकड़ों में बांटने के लिए। उप-स्ट्रिंग निकालने के लिए `substr` सबसे अधिक लचीला है: आप शुरुआत और लंबाई निर्दिष्ट कर सकते हैं।

## See Also (और भी देखें):

- PHP Manual on `substr`: [php.net/manual/en/function.substr.php](https://php.net/manual/en/function.substr.php)
- PHP Manual on `mb_substr` for multibyte strings: [php.net/manual/en/function.mb-substr.php](https://php.net/manual/en/function.mb-substr.php)
- String Functions in PHP: [php.net/manual/en/ref.strings.php](https://php.net/manual/en/ref.strings.php)
