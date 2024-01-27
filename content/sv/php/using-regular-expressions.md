---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för att matcha textsträngar. Programmerare använder dem för sökning, ersättning och datavalidering, det sparar tid och kodrader.

## Hur gör man:
```PHP
<?php
// Hitta alla ord som börjar med 'b' och följs av 'at'
$pattern = '/\bbat\b/';
$text = "The bat went splat at the brat.";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Output: Array ( [0] => bat )

// Ersätta alla siffror med '#'
$text = "Husnummer 123B, Lägenhet 45.";
$replacedText = preg_replace('/\d/', '#', $text);
echo $replacedText;
// Output: Husnummer ###B, Lägenhet ##.
?>
```

## Deep Dive
Reguljära uttryck härstammar från teoretisk datavetenskap och formaliserades på 1950-talet. Alternativ till regex är oftast inbyggda strängfunktioner som `strpos()` eller `str_replace()`, men de är mindre kraftfulla. PHP använder PERL-stil regex-mönster, och funktioner som `preg_match` under huven använder PCRE-biblioteket (Perl Compatible Regular Expressions).

## Se Också
- PHP Manual on preg_match: https://www.php.net/manual/en/function.preg-match.php
- PHP Manual on preg_replace: https://www.php.net/manual/en/function.preg-replace.php
- Regex tester och läromaterial: https://regexr.com/
- PCRE dokumentation: https://www.pcre.org/
