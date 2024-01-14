---
title:    "PHP: Kontrollera om en mapp finns"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en vanlig uppgift för många PHP-utvecklare. Det kan vara till nytta när man till exempel behöver lägga till filer i en specifik mapp eller för att se om ett installationssteg har implementerats korrekt. Oavsett anledning är det viktigt att veta hur man enkelt kan utföra denna uppgift i PHP.

## Hur man

För att kontrollera om en mapp existerar i PHP används funktionen `file_exists()`. Den här funktionen tar en sökväg som argument och returnerar `true` om sökvägen existerar och `false` om den inte gör det.

```PHP
if (file_exists("/mapp/exempel")) {
    echo "Mappen existerar";
} else {
    echo "Mappen existerar inte";
}
```

Om mappen existerar kommer "Mappen existerar" att skrivas ut i konsolen, annars kommer "Mappen existerar inte" att skrivas ut.

## Deep Dive

För att förstå hur `file_exists()`-funktionen fungerar djupare kan vi titta på dess returvärde. Om det returnerade värdet är `true` betyder det att sökvägen finns och PHP-programmet kan fortsätta att exekvera. Om det returnerade värdet är `false` innebär det att sökvägen inte finns och att speciella åtgärder kan behövas för att hantera detta.

Det är också viktigt att notera att `file_exists()`-funktionen inte bara fungerar för mappar, utan kan även användas för att kontrollera om en fil existerar.

## Se också

- [PHP filsystemfunktioner](https://www.php.net/manual/en/ref.filesystem.php)
- [Kontrollera om en fil existerar i PHP](https://www.domainit.com/help/topic/052/how-to-check-if-a-file-exists-in-php/)
- [Hitta mappens sökväg i PHP](https://stackoverflow.com/questions/45792601/how-can-i-get-path-of-my-directory-in-php)