---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:43:09.475446-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

पैटर्न से मेल खाते कैरक्टर्स को हटाना मतलब विशेष तरह के कैरक्टर्स को स्ट्रिंग से निकाल देना। प्रोग्रामर इसे डेटा सफाई, वैधीकरण या फॉर्मेटिंग के लिए करते हैं।

## How to: (कैसे करें:)

PHP में, आप `preg_replace()` फंक्शन का उपयोग करके पैटर्न मैचिंग करके कैरक्टर्स डिलीट कर सकते हैं।

```PHP
<?php
$text = "नमस्ते! कैसे हैं आप? 123";
$pattern = '/[0-9]+/'; // संख्याओं को ढूंढने का पैटर्न

$cleaned_text = preg_replace($pattern, '', $text);
echo $cleaned_text; // आउटपुट: "नमस्ते! कैसे हैं आप? "
?>
```

## Deep Dive (गहराई से विवेचना)

`preg_replace()` फंक्शन PCRE (Perl Compatible Regular Expressions) का इस्तेमाल करता है। यह पहले PHP 4 में आया था, और अब PHP 7 और PHP 8 में भी है। अल्टरनेटिव में आप `str_replace()` या `str_ireplace()` का इस्तेमाल कर सकते हैं, अगर सिर्फ साधारण स्ट्रिंग्स को रिप्लेस करना हो। लेकिन जब पैटर्न मैचिंग की आवश्यकता हो, `preg_replace()` ज्यादा शक्तिशाली होता है। इसके इंटरनल इम्प्लिमेंटेशन में यह "backtracking" एल्गोरिथ्म का उपयोग करता है, जो पैटर्न को मैच करने के लिए इधर-उधर देखता है।

## See Also (और भी देखें)

- PHP official documentation for preg_replace: [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- Regular expressions tutorial: [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)
- PHP Regular Expressions (PCRE): [PHP: PCRE - Manual](https://www.php.net/manual/en/book.pcre.php)
