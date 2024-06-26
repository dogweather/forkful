---
date: 2024-01-20 17:58:20.903771-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PHP \u092E\
  \u0947\u0902 text search \u0914\u0930 replace \u0915\u0947 \u0932\u093F\u090F `str_replace`\
  \ function \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:54.441276-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PHP \u092E\u0947\u0902\
  \ text search \u0914\u0930 replace \u0915\u0947 \u0932\u093F\u090F `str_replace`\
  \ function \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
PHP में text search और replace के लिए `str_replace` function का इस्तेमाल करते हैं:

```PHP
<?php
$originalText = 'Hello World';
$search = 'World';
$replaceWith = 'PHP';
$newText = str_replace($search, $replaceWith, $originalText);

echo $newText;  // Output: Hello PHP
?>
```

Regular expressions के लिए `preg_replace` function का इस्तेमाल करते हैं:

```PHP
<?php
$originalText = 'The quick brown fox jumps over the lazy dog';
$pattern = '/brown fox/';
$replacement = 'black cat';
$newText = preg_replace($pattern, $replacement, $originalText);

echo $newText;  // Output: The quick black cat jumps over the lazy dog
?>
```

## Deep Dive (गहराई में जानकारी):
Search और replace functionality PHP में बहुत पहले से है। `str_replace` simple strings के लिए है, जबकि `preg_replace` complex patterns (regular expressions) के लिए है। Alternatives में string functions जैसे के `strpos`, और `substr_replace` शामिल हैं। Performance-wise, `str_replace` `preg_replace` से तेज़ होता है जब patterns simple हों, क्योंकि regular expressions CPU को ज्यादा use करते हैं।

## See Also (देखें भी):
- PHP official documentation on `str_replace`: [php.net/manual/en/function.str-replace.php](https://www.php.net/manual/en/function.str-replace.php)
- PHP official documentation on `preg_replace`: [php.net/manual/en/function.preg-replace.php](https://www.php.net/manual/en/function.preg-replace.php)
- Regular Expressions Basic Syntax: [regular-expressions.info](https://www.regular-expressions.info/)
- `strpos` for finding the position of a substring in a string: [php.net/manual/en/function.strpos.php](https://www.php.net/manual/en/function.strpos.php)
- `substr_replace` for replacing text within a string: [php.net/manual/en/function.substr-replace.php](https://www.php.net/manual/en/function.substr-replace.php)
