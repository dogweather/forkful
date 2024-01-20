---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Konkatenering av strängar är processen att sätta ihop två eller flera strängar i PHP. Programmerare gör detta för att skapa dynamiska strängar eller skriva ut kombinerade värden.

## Hur gör man:

PHP använder punkt (`.`) operatorn för att konkatenera strängar. Mycket rakt på sak, titta på det här:

```PHP  
<?php
    // Det här är våra strängar
    $str1 = "Hej";
    $str2 = " Sverige";
    // Konkatenerar dem
    $tillsammans = $str1 . $str2;
    // Skriver ut resultaten
    echo $tillsammans;
?>
```

Testa detta i din PHP-editor, det kommer att skriva ut: 

```
Hej Sverige
```

## Djupare dykning

Historiskt sett har PHP alltid använt `.` operatören för strängkonkatenering, vilket skiljer sig från andra språk som t.ex. JavaScript som använder `+` operatören. Men kom ihåg att om du försöker konkatenera med `+` i PHP kommer det att tolkas som addition, inte konkatenering!

Alternativt kan du använda `.=`, vilket sparar några tangenttryckningar och gör koden lite renare:

```PHP 
<?php
    $str1 = "Hej";
    // Läggs direkt till i $str1
    $str1 .= " Sverige";
    echo $str1;
?>
```

Denna kod skriver också ut `Hej Sverige`.

PHP hanterar internt denna konkatenering genom att skapa en ny sträng och kopiera innehållet från de ursprungliga strängarna till den nya strängen. Detta kan vara onödigt dyrt i termer av minne när strängarna är stora.

## Se även

1. [PHP String-operators på PHP.net](https://www.php.net/manual/en/language.operators.string.php)
2. [PHP strängmanipulation på W3Schools](https://www.w3schools.com/php/php_ref_string.asp)