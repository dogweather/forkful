---
title:                "Converting a string to lower case"
date:                  2024-01-20T17:38:53.131250-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a string to lower case"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

In PHP, converting a string to lower case means transforming all alphabetical characters in a string to their lowercase variant. Programmers do this for consistency, especially when comparing or sorting strings, where case-sensitivity could mess things up.

## How to:

PHP uses `strtolower` to make all characters in a string lower case. Here’s how it works:

```php
<?php
$originalString = "HeLLo WoRLD!";
$lowerCaseString = strtolower($originalString);

echo $lowerCaseString; // Outputs: hello world!
?>
```

If you need to handle multibyte character encodings, like UTF-8, use `mb_strtolower` instead:

```php
<?php
$originalString = "İstanbul";
$lowerCaseString = mb_strtolower($originalString, 'UTF-8');

echo $lowerCaseString; // Outputs: istanbul (correctly converts İ to i)
?>
```

## Deep Dive

Historically, PHP's `strtolower` function has been the go-to function for case conversion, introduced in very early versions of PHP. However, as PHP applications became more global, the need to correctly handle multibyte character encodings brought about `mb_strtolower`.

Alternatives to `strtolower` and `mb_strtolower` include using regular expressions with the `mb_ereg_replace_callback` function or `preg_replace_callback`, but for simple case conversion, they are overkill.

In PHP, strings have traditionally been byte-based, not character-based, meaning each byte is one character. This works for single-byte encodings like ASCII, where each character indeed is one byte. For multibyte encodings, `mb_strtolower` understands character encoding and treats characters as they should be treated.

## See Also

- PHP Manual on `strtolower`: https://www.php.net/manual/en/function.strtolower.php
- PHP Manual on `mb_strtolower`: https://www.php.net/manual/en/function.mb-strtolower.php
- UTF-8 and Unicode for PHP devs: https://www.php.net/manual/en/book.mbstring.php
