---
title:                "पाठ खोजना और बदलना"
aliases: - /hi/php/searching-and-replacing-text.md
date:                  2024-01-20T17:58:20.903771-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Searching and replacing text in PHP मतलब है दिए गए टेक्स्ट को ढूँढना और बदलना। Programmers इसे करते हैं डेटा को फॉर्मेट या अपडेट करने के लिए, जैसे किसी document में कोई शब्द बदलना।

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
