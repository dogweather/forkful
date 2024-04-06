---
date: 2024-01-20 17:58:28.465298-07:00
description: "How to (Hur man g\xF6r) I PHP \xE4r `str_replace()` en v\xE4n i n\xF6\
  den f\xF6r att byta textstr\xE4ngar. H\xE4r \xE4r ett exempel."
lastmod: '2024-04-05T22:37:46.679161-06:00'
model: gpt-4-1106-preview
summary: "How to (Hur man g\xF6r) I PHP \xE4r `str_replace()` en v\xE4n i n\xF6den\
  \ f\xF6r att byta textstr\xE4ngar. H\xE4r \xE4r ett exempel."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## How to (Hur man gör)
I PHP är `str_replace()` en vän i nöden för att byta textsträngar. Här är ett exempel:

```php
<?php
$originalText = "Hej, världen!";
$replacedText = str_replace("världen", "Sverige", $originalText);
echo $replacedText; // Skriver ut: Hej, Sverige!
?>
```

Vill du byta ut mer komplexa mönster? `preg_replace()` använder reguljära uttryck:

```php
<?php
$originalText = "Hunden hoppar högt 2023!";
$replacedText = preg_replace("/\d+/", "2024", $originalText);
echo $replacedText; // Skriver ut: Hunden hoppar högt 2024!
?>
```

## Deep Dive (Djupdykning)
`str_replace()` existar sedan PHP 4 och är snabb för enkla ersättningar. För mer avancerade behov finns `preg_replace()`, som kom till i PHP 3 och använder Perl-kompatibla reguljära uttryck.

Alternativ? För större textmängder kan `strtr()` vara effektivare, och `str_ireplace()` erbjuder case-insensitive sökfunktion.

När det gäller implementation, kom ihåg: `preg_replace()` kan bli långsamt med komplexa uttryck eller stora datamängder. Och se upp med 'backreferences' och gruppering i dina reguljära uttryck – det kan ställa till det.

## See Also (Se även)
- PHP Manual on `str_replace()`: https://www.php.net/manual/en/function.str-replace.php
- PHP Manual on `preg_replace()`: https://www.php.net/manual/en/function.preg-replace.php
- Regular Expressions (RegEx) Tutorial: https://www.regular-expressions.info/
- PHP `strtr()` function documentation: https://www.php.net/manual/en/function.strtr.php
