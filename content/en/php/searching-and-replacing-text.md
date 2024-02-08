---
title:                "Searching and replacing text"
aliases:
- en/php/searching-and-replacing-text.md
date:                  2024-01-20T17:58:27.807114-07:00
model:                 gpt-4-1106-preview
simple_title:         "Searching and replacing text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is how you find specific strings in content and swap them out for something else. Programmers do this to update data, fix errors, or alter text en masse without manual edits.

## How to:

Here's a quick way to replace 'cat' with 'dog' in a sentence using PHP:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat';
$replacedText = str_replace('cat', 'dog', $text);

echo $replacedText;
?>
```

Sample Output:

```
The quick brown fox jumps over the lazy dog
```

Now, suppose we're dealing with case-insensitive replacement:

```PHP
<?php
$text = 'Catapults are CATegorically amazing!';
$replacedText = str_ireplace('cat', 'dog', $text);

echo $replacedText;
?>
```

Sample Output:

```
Dogapults are DOGegorically amazing!
```

## Deep Dive:

Search and replace functions have been around since the early days of computing — think `sed` in Unix. In PHP, `str_replace` and `str_ireplace` are your go-to for a simple search and replace. `str_replace` is case-sensitive, while `str_ireplace` isn't.

How do they work? Under the hood, both functions check each part of the string, look for matches, and replace them. They handle arrays too, so you can search and replace multiple patterns in one go.

Now, if you need more control, like pattern matching, you'll want to use `preg_replace`. This utilizes regular expressions, offering much more flexibility and precision:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat 7 times.';
$replacedText = preg_replace('/\bcat\b/i', 'dog', $text);

echo $replacedText;
?>
```

Sample Output:

```
The quick brown fox jumps over the lazy dog 7 times.
```

This replaced 'cat' with 'dog', ignoring case (`/i` modifier), and matched whole words only (`\b` word boundary).

## See Also:

- PHP Official Documentation on str_replace: https://www.php.net/manual/en/function.str-replace.php
- PHP Official Documentation on str_ireplace: https://www.php.net/manual/en/function.str-ireplace.php
- PHP Official Documentation on preg_replace: https://www.php.net/manual/en/function.preg-replace.php
- Regular Expressions Tutorial: https://www.regular-expressions.info/
- Unix `sed` stream editor for filter and transform text: http://www.grymoire.com/Unix/Sed.html
