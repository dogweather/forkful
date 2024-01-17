---
title:                "Att hitta längden av en sträng"
html_title:           "PHP: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
När man programmerar i PHP finns det ofta behov av att hitta längden på en sträng. Detta är helt enkelt antalet tecken som finns i en viss sträng. Detta kan vara användbart för att till exempel kontrollera om en inmatad sträng är tillräckligt lång eller för att trimma bort onödiga mellanslag i en sträng.

## Hur man gör:
För att hitta längden på en sträng i PHP använder man funktionen ```strlen()```. Denna funktion tar emot en sträng som argument och returnerar längden på strängen som en heltalsvärde. Se nedan för ett enkelt exempel:

```PHP
$string = "Hej! Detta är en sträng.";
$lenght = strlen($string);
echo $length; // Skriver ut 24 vilket är antalet tecken i strängen.
```

## Djupdykning:
För att förstå varför det är nödvändigt att hitta längden på en sträng behöver man förstå lite om hur PHP behandlar strängar. I äldre versioner av PHP var alla strängar en del av ett annat datatyp som hette array. Detta gjorde att man kunde använda array-funktioner på strängar, som till exempel funktionen ```count()```. Men i moderna versioner är strängar en egen datatyp och därför behövs en specifik funktion för att räkna antalet tecken i en sträng.

Det finns också alternativa sätt att hitta längden på en sträng i PHP. Man kan till exempel använda funktionen ```mb_strlen()``` som hanterar flerspråkiga strängar på ett bättre sätt. Det finns också möjlighet att använda en for-loop för att räkna antalet tecken i en sträng, men detta är inte lika effektivt som att använda den inbyggda funktionen ```strlen()```.

## Se även:
- [PHP Manual - strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual - mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHP Array Functions](https://www.w3schools.com/php/php_ref_array.asp)