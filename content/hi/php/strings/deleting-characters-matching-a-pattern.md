---
aliases:
- /hi/php/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:43:09.475446-07:00
description: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932\
  \ \u0916\u093E\u0924\u0947 \u0915\u0948\u0930\u0915\u094D\u091F\u0930\u094D\u0938\
  \ \u0915\u094B \u0939\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0935\u093F\
  \u0936\u0947\u0937 \u0924\u0930\u0939 \u0915\u0947 \u0915\u0948\u0930\u0915\u094D\
  \u091F\u0930\u094D\u0938 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u0938\u0947 \u0928\u093F\u0915\u093E\u0932 \u0926\u0947\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0921\u0947\u091F\u093E \u0938\u092B\u093E\u0908, \u0935\u0948\u0927\u0940\u0915\
  \u0930\u0923 \u092F\u093E \u092B\u0949\u0930\u094D\u092E\u0947\u091F\u093F\u0902\
  \u0917 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947\u2026"
lastmod: 2024-02-18 23:09:03.471211
model: gpt-4-1106-preview
summary: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0915\u0948\u0930\u0915\u094D\u091F\u0930\u094D\u0938 \u0915\
  \u094B \u0939\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0935\u093F\u0936\
  \u0947\u0937 \u0924\u0930\u0939 \u0915\u0947 \u0915\u0948\u0930\u0915\u094D\u091F\
  \u0930\u094D\u0938 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0938\u0947 \u0928\u093F\u0915\u093E\u0932 \u0926\u0947\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\
  \u0947\u091F\u093E \u0938\u092B\u093E\u0908, \u0935\u0948\u0927\u0940\u0915\u0930\
  \u0923 \u092F\u093E \u092B\u0949\u0930\u094D\u092E\u0947\u091F\u093F\u0902\u0917\
  \ \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947\u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
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
