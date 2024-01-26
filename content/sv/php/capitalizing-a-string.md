---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla dess första bokstav till versal. Programmerare gör detta för att följa konventioner eller förbättra läsbarheten; till exempel, att börja meningar med en stor bokstav eller formatera rubriker.

## Hur man gör:
```PHP
<?php
// Sträng med små bokstäver
$exampleString = "här är en sträng";

// Kapitalisera första bokstaven i strängen
$capitalizedString = ucfirst($exampleString);

// Skriv ut den kapitaliserade strängen
echo $capitalizedString; // Output: Här är en sträng

// Kapitalisera varje ords första bokstav
$titleString = "detta är en titel";

// Använd ucwords för att kapitalisera varje ord
$capitalizedTitle = ucwords($titleString);

// Skriv ut den kapitaliserade titeln
echo $capitalizedTitle; // Output: Detta Är En Titel
?>
```

## Djupdykning:
Tidigare, när typsnitt var begränsade och datautrymme dyrbart, sparades ofta varje byte. Strängar lagrades i små bokstäver för att vara konsekventa. Idag är kapitalisering ett enkelt sätt att göra text mer användarvänlig utan större kostnad.

`ucfirst` och `ucwords` är de vanligaste funktionerna i PHP för att manipulera strängkapitalisering. Båda behandlar UTF-8-strängar korrekt från PHP 5.4.0 och framåt. Det finns alternativ såsom `mb_convert_case()` som kan hantera multibyte-strängar för språk med accenttecken och andra icke-latinska teckensnitt.

Tekniskt sett skapar `ucfirst` en ny sträng med den första bokstaven omvandlad till versal, medan resten av strängen förblir oförändrad. `ucwords` kapitaliserar första bokstaven i varje ord i strängen. Var försiktig med `ucwords` eftersom den betraktar alla tecken efter ett icke-alfabetiskt tecken som början på ett nytt ord.

## Se även:
- PHPs officiella dokumentation om [ucfirst](https://www.php.net/manual/en/function.ucfirst.php)
- PHPs officiella dokumentation om [ucwords](https://www.php.net/manual/en/function.ucwords.php)
- Stack Overflow diskussioner kring praktiska användningsfall och problem med strängkapitalisering i PHP.
- W3Schools tutorial om [PHP String Functions](https://www.w3schools.com/php/php_ref_string.asp), där dessa och andra liknande funktioner beskrivs.
