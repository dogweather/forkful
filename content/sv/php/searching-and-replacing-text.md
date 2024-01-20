---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Sök och ersätt text med PHP

## Vad & Varför?

Sök och ersätt är en teknik för att hitta specifik text och byta ut den mot något annat i koden. Programmerare använder den för att göra massändringar, korrigera fel, uppdatera data och generellt förbättra koden.

## Hur man:

Du kan söka och ersätta text i PHP med hjälp av `str_replace` funktionen. Här är ett exempel på hur du använder den:

```PHP
<?php
$string  = "Hej! Det är bra att se dig!";
$changed_string = str_replace("bra", "fantastisk", $string);
echo $changed_string;
?>
```

Output:

```
Hej! Det är fantastisk att se dig!
```

I det här exemplet söker vi efter "bra" i `$string` och ersätter det med "fantastisk". Resultatet blir alltså "Hej! Det är fantastisk att se dig!".

## Fördjupning

`str_replace` funktionen har sitt ursprung i C programmeringsspråket och är till för att underlätta strängmanipuleringar. Du kan också utföra mer avancerade sök- och ersättoperationer med hjälp av regex (reguljära uttryck), via PHP's `preg_replace` funktion. Dessutom kan du även använda `str_ireplace` för case-insensitive sökningar. Implementationsdetaljer får du löpande utforska själv, men mjuka övergångar och snabba operationer är viktiga faktorer att hålla koll på.

## Se även

Du hittar mer information om strängfunktioner och manipulation på följande platser:

- PHP's officiella dokumentation om `str_replace`: [http://php.net/manual/en/function.str-replace.php](http://php.net/manual/en/function.str-replace.php)
- W3Schools tillhandahåller grundläggande och praktiska exempel på sök och ersätt: [https://www.w3schools.com/php/func_string_str_replace.asp](https://www.w3schools.com/php/func_string_str_replace.asp)
- Stack Overflow samhället har en mängd diskussioner kring ämnet, se till exempel: [https://stackoverflow.com/questions/1252693/using-str-replace-so-that-it-only-acts-on-the-first-match](https://stackoverflow.com/questions/1252693/using-str-replace-so-that-it-only-acts-on-the-first-match)

Ses nästa gång!