---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att ta bort tecken enligt ett visst mönster är en frekvent uppgift i programmering där du specifierar en grupp tecken att eliminera från en sträng. Programmerare gör detta för att rensa data, format om text, eller för att hitta subtexter.

## Så här gör du:

Här ska vi använda PHP funktionen `preg_replace()`. Här är ett exempel:

```PHP
<?php
$text = 'Hej, Världen!123';
$pattern = '/[^a-zA-ZåäöÅÄÖ\s,.!?]/';
$clean_text = preg_replace($pattern, '', $text);
echo $clean_text;
?>
```

I detta fall, visar output: `'Hej, Världen!'`, då allt som inte är en bokstav, mellanslag, komma, punkt, utropstecken eller frågetecken har tagits bort.

## Fördjupning

Historiskt sett är regelbundna uttryck (som vi just använde) en funktion som går tillbaka till 1950-talet och har implementerats i många programmeringsspråk. 

Ett alternativ till `preg_replace()` är `str_replace()` när du bara behöver ersätta specifika tecken. Till exempel:

```PHP
<?php
$text = "Hej, Världen!123";
$search = "123";
$replace = "";
$result = str_replace($search, $replace, $text);
echo $result;
?>
```
PHP utför dessa operationer genom att först transformera mönstret till en automatisk maskin och sedan gå igenom strängen symbol för symbol, vilket gör det både snabbt och effektivt.

## Se även

1. PHP officiella dokumentation om str_replace: https://php.net/manual/en/function.str-replace.php
2. PHP officiella dokumentation om preg_replace: https://php.net/manual/en/function.preg-replace.php

För mer avancerade ämnen, rekommenderas PHP 's officiella dokumentation om regelbundna uttryck: https://www.php.net/manual/en/book.pcre.php