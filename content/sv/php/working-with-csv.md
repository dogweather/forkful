---
title:                "Arbeta med csv"
html_title:           "PHP: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jobba med CSV-filer handlar om att hantera data på en strukturerad och enkel sätt. Dessa filer kan innehålla tusentals rader med information, och med hjälp av PHP kan vi effektivt extrahera och bearbeta denna data. Det är ett vanligt verktyg för många programmerare när de behöver hantera stora datamängder.

## Hur:
Att arbeta med CSV-filer i PHP är väldigt enkelt och kräver bara några få steg. Först behöver vi öppna filen med funktionen `fopen()` och sedan läsa in datan rad för rad med `fgets()`. Därefter kan vi använda inbyggda PHP-funktioner som `explode()` för att dela den inlästa datan i separata fält. Vi kan också loopa igenom datan och utföra olika operationer på den, exempelvis byta ut värden eller lägga till ny information.

```
<?php

// Öppna CSV-filen för läsning
$csv = fopen('data.csv', 'r');

// Loopa igenom varje rad i filen
while (($rad = fgets($csv)) !== false) {
    // Dela upp raden baserat på kommatecken
    $falt = explode(',', $rad);
    // Gör något med datan, exempelvis skriv ut på skärmen
    echo $falt[0] . ' är ' . $falt[1] . ' år gammal.';
}

// Stäng filen
fclose($csv);
```

Output:
```
Johan är 32 år gammal.
Lisa är 28 år gammal.
```

## Djupdykning:
CSV (Comma-Separated Values) formatet har funnits sedan 1970-talet och används fortfarande flitigt idag för att enkelt hantera stora datamängder. Det är ett populärt format för att överföra data mellan olika program och plattformar. Alternativ till CSV inkluderar XML och json, men CSV är fortfarande ett vanligt val på grund av sin enkla och lättlästa struktur.

Förutom att läsa in och manipulera data från en CSV-fil, kan PHP även skapa och skriva till CSV-filer med hjälp av funktioner som `fputcsv()` och `fputcsv()`. Det finns också olika tredjepartsbibliotek som kan hjälpa till med att hantera CSV-data på ett mer avancerat sätt, till exempel hantera stora datamängder.

## Se även:
- [PHP manual: CSV functions](http://php.net/manual/en/ref.filesystem.php)
- [PHP for beginners: How to read a CSV file with PHP](https://phpforbeginners.com/working-with-csv-files/)
- [PHP League CSV: A PHP library for reading and writing CSV files](https://csv.thephpleague.com/)