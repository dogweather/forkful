---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

किसी पैटर्न/प्रतिमान से मेल खाने वाले वर्णों को हटाना मतलब है PHP में ऐसे वर्णों को संशोधन करना जो एक निर्दिष्ट प्रतिमान/पैटर्न से मेल खा रहे हों। प्रोग्रामर्स इसे आंकड़ों से निपटने, डाटा सफाई और बेहतर इनपुट प्रबंधन के लिए करते हैं।

## कैसे करें:

PHP में, हम `preg_replace()` का उपयोग कर सकते हैं। यहां एक उदाहरण है:

```PHP
<?php
$string = "Hello, World!";
$string_new = preg_replace("/[^a-zA-Z0-9]/", "", $string);
echo $string_new;
?>
```

आउटपुट होगा:

```
HelloWorld
```

## गहराई में जाएं:

"Deleting characters matching a pattern" का विधान PHP 4.0 और PHP 5.0 के बीच में आया था। इससे पहले इसे बीटा बाई बीट एक्सेस मेथड का उपयोग करके किया जाता था। 

आप substr_replace() और str_replace() जैसे विकल्प भी उपयोग कर सकते हैं, लेकिन preg_replace() बेहद शक्तिशाली है क्योंकि यह नियमित अभिव्यक्तियों का समर्थन करता है।

हम प्रतियोगी "str_replace" का उपयोग नीचे दिए गए कोड में देख सकते हैं:

```PHP
<?php
$string = "Hello, World!";
$string_new = str_replace(' ', '', $string);
echo $string_new;
?>
```

आउटपुट होगा:

```
Hello,World!
```

## अन्य देखें:

1. `str_replace()` का PHP प्रलेखन: [https://www.php.net/manual/en/function.str-replace.php](https://www.php.net/manual/en/function.str-replace.php)
2. `preg_replace()` का PHP प्रलेखन: [https://www.php.net/manual/en/function.preg-replace.php](https://www.php.net/manual/en/function.preg-replace.php)