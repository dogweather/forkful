---
title:                "स्ट्रिंग को जोड़ना"
aliases: - /hi/php/concatenating-strings.md
date:                  2024-01-20T17:36:00.774201-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
PHP में स्ट्रिंग कन्केटनेशन यानी स्ट्रिंग्स को जोड़ना, जब हमें अलग अलग टेक्स्ट के टुकड़ों को एक साथ मिलाना होता है। डेटा डिस्प्ले करते समय या डायनैमिक टेक्स्ट बनाते समय यह बेहद उपयोगी होता है।

## How to (कैसे करें):
PHP में स्ट्रिंग्स को जोड़ने के लिए हमें डॉट (.) ऑपरेटर का इस्तेमाल करना पड़ता है। आइए देखे कुछ उदाहरण:

```PHP
<?php
// स्ट्रिंग्स को जोड़ना
$greeting = "नमस्ते";
$name = "विश्व";

$fullGreeting = $greeting . " " . $name . "!";
echo $fullGreeting; // नमस्ते विश्व!
?>
```
आप वेरिएबल्स और टेक्स्ट को मिलाकर भी कन्केटनेट कर सकते हैं:
```PHP
<?php
echo "हैलो, " . $name . "! आप कैसे हैं?"; // हैलो, विश्व! आप कैसे हैं?
?>
```

## Deep Dive (गहराई में जानकारी):
PHP में स्ट्रिंग कन्केटनेशन की सुविधा PHP4 से ही उपलब्ध है। पहले लोग वेरिएबल्स को सीधे एक-दूसरे के बाद लिखते थे, लेकिन डॉट ऑपरेटर ने इसे और आसान बना दिया। PHP में स्ट्रिंग को जोड़ने के विकल्प भी हैं जैसे कि `sprintf()` या हेरेडोक/नाउडोक सिंटैक्स, पर डॉट ऑपरेटर सबसे ज्यादा सीधा और आसान है। जब स्ट्रिंग्स को जोड़ते हैं, PHP एक नई स्ट्रिंग बनाती है और ओरिजिनल स्ट्रिंग्स को अपरिवर्तित रखती है।

## See Also (इसे भी देखें):
- PHP स्ट्रिंग हैंडलिंग: https://www.php.net/manual/en/book.strings.php
- PHP `sprintf()`: https://www.php.net/manual/en/function.sprintf.php
- PHP String Operators: https://www.php.net/manual/en/language.operators.string.php
- Heredoc syntax in PHP: https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc
