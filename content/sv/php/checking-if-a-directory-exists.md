---
title:    "PHP: Kontrollera om en mapp finns"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför
Att kontrollera om en mapp existerar är en vanlig uppgift i PHP-programmering. Det är viktigt att kunna verifiera att en mapp finns innan man försöker arbeta med dess innehåll, t.ex. för att undvika felmeddelanden eller felaktiga operationer.

## Hur man gör det
För att kontrollera om en mapp existerar i PHP använder man sig av `is_dir()` funktionen. Nedan finns ett exempel på hur man kan använda den:

```PHP
$path = "/path/to/directory";

if (is_dir($path)) {
    echo "$path existerar!";
} else {
    echo "$path existerar inte!";
}
```

Om mappen existerar så kommer "existerar!" att skrivas ut, annars kommer "existerar inte!" att skrivas ut.

## Djupdykning
För att förstå hur `is_dir()` funktionen fungerar, är det viktigt att veta att den jämför angiven sökväg med det aktuella filsystemet. Om sökvägen inte börjar med ett `/` kommer funktionen att använda sig av det aktuella arbetsmappet för att bygga den fullständiga sökvägen.

En annan viktig punkt är att `is_dir()` returnerar True även om sökvägen pekar på en symbolisk länk till en mapp. För att kontrollera om sökvägen pekar på en faktisk mapp, kan man använda sig av `is_link()` funktionen först och därefter kontrollera om sökvägen pekar på en mapp.

## Se även
- [PHP manual för is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [PHP manual för is_link()](https://www.php.net/manual/en/function.is-link.php)