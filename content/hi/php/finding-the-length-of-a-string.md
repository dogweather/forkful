---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:48:38.316062-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग की लंबाई पता लगाने का मतलब है कि हम स्ट्रिंग में मौजूद अक्षरों की संख्या जानना चाहते हैं। प्रोग्रामर्स इसका इस्तेमाल इनपुट वैलिडेशन, डाटा संसाधन और यूजर इंटरफेस डिजाइन के वक्त करते हैं।

## कैसे करें:
PHP में स्ट्रिंग की लंबाई पता करने के लिए `strlen()` फंक्शन का उपयोग करें। नीचे कोड देखें:

```php
<?php
$text = "नमस्ते दुनिया";
$length = strlen($text);
echo "स्ट्रिंग की लंबाई: " . $length;
?>
```

सैंपल आउटपुट:

```
स्ट्रिंग की लंबाई: 27
```

## गहराई में:
`strlen()` PHP में एक बहुत प्राचीन और मौलिक फंक्शन है। यह बाइट्स की संख्या लौटाता है, ना कि वर्णों की, जो UTF-8 जैसे मल्टीबाइट कैरेक्टर एनकोडिंग में एक विचारणीय मुद्दा हो सकता है, क्योंकि एक वर्ण कई बाइट्स का हो सकता है। ऐसे मामलों में `mb_strlen()` फंक्शन का उपयोग करना बेहतर होता है। उदाहरण के लिए:

```php
<?php
$text = "नमस्ते दुनिया";
$length = mb_strlen($text);
echo "स्ट्रिंग की लंबाई: " . $length; // बेहतर नतीजे के लिए
?>
```

`mb_strlen()` आपको सही चरित्र गणना प्रदान करेगा, जब आप मल्टीबाइट कैरेक्टर सेट्स का सामना कर रहे हों।

## और भी:
- PHP ऑफिसियल `strlen()` डॉक्युमेंटेशन: [php.net/manual/en/function.strlen.php](https://www.php.net/manual/en/function.strlen.php)
- PHP मल्टीबाइट स्ट्रिंग फंक्शन्स: [php.net/manual/en/ref.mbstring.php](https://www.php.net/manual/en/ref.mbstring.php)
- UTF-8 एनकोडिंग और PHP का उपयोग कैसे करें: [php.net/manual/en/book.mbstring.php](https://www.php.net/manual/en/book.mbstring.php)
