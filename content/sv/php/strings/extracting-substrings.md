---
date: 2024-01-20 17:46:35.113330-07:00
description: "How to: PHP erbjuder olika funktioner f\xF6r att hantera substr\xE4\
  ngar. `substr()` \xE4r mest anv\xE4nd."
lastmod: '2024-03-13T22:44:37.985585-06:00'
model: gpt-4-1106-preview
summary: "PHP erbjuder olika funktioner f\xF6r att hantera substr\xE4ngar."
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## How to:
PHP erbjuder olika funktioner för att hantera substrängar. `substr()` är mest använd:

```php
$str = "Hej, världen!";
echo substr($str, 4, 7); // output: "världen"
```

En till funktion är `mb_substr()`, vilket är bättre för multibyte(strängar i UTF-8 till exempel):

```php
$str = "Färgglad sträng";
echo mb_substr($str, 0, 9); // output: "Färgglad"
```

Man kan även använda `strpos()` och `substr()` tillsammans för att hitta och extrahera en del av en sträng:

```php
$str = "Hitta en nål i en höstack";
$needle = "nål";
$pos = strpos($str, $needle);

if ($pos !== false) {
    echo substr($str, $pos, strlen($needle)); // output: "nål"
}
```

## Deep Dive:
Extraktion av substrängar är en grundläggande strängoperation och har varit en del av PHP sedan de tidiga versionerna. Det är en del av nästan varje programmeringsspråks standardbibliotek.

Alternativ till `substr()` och `mb_substr()` inkluderar `strstr()`, `strchr()`, och reguljära uttryck med `preg_match()` eller `preg_match_all()`. Dessa kan användas när mer komplexa matchningsmönster behövs.

I utförandet använder PHPs strängfunktioner `zend_string`-strukturen internt, som hanterar minnesallokering och kapacitetsförändringar när substrängar tas bort från eller läggs till i strängar.

## See Also:
- PHP Manual on String Functions: https://www.php.net/manual/en/ref.strings.php
- PHP Manual on `substr()`: https://www.php.net/manual/en/function.substr.php
- PHP Manual on `mb_substr()`: https://www.php.net/manual/en/function.mb-substr.php
- PHP Manual on `strstr()`: https://www.php.net/manual/en/function.strstr.php
- Regular Expressions (PCRE) in PHP: https://www.php.net/manual/en/book.pcre.php
