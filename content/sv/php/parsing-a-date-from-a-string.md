---
title:                "Parsa ett datum från en sträng"
html_title:           "PHP: Parsa ett datum från en sträng"
simple_title:         "Parsa ett datum från en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att "parsa" ett datum från en sträng innebär att man omvandlar en textsträng som innehåller en datumtidsinformation till en mer läsbar och användbar format. Programmörer gör det ofta för att kunna sortera och filtrera datum i sina applikationer och webbsidor.

## Hur man gör:

```PHP
// Exempel på en sträng med datumtidsinformation:
$dateString = "2020-10-31 12:00:00";

// Omvandla strängen till ett datumobjekt:
$dateTime = strtotime($dateString);

// Skriva ut datumet i ett önskat format:
echo date('d/m/Y h:i A', $dateTime);

// Resultat: 31/10/2020 12:00 PM
```

## Djupdykning:

Att parsadata från en sträng är en vanlig uppgift för programmerare och det finns många olika sätt att göra det på. Innan PHP version 5.2 så användes funktionen `strtotime()`som nu har ersatts av`DateTime` objekt. Det finns också tillgängliga tredjepartsbibliotek för mer avancerad datumtidsmanipulering, som till exempel Carbon.

## Se även:

- PHP officiell dokumentation för `strtotime()` och `DateTime`: https://www.php.net/manual/en/function.strtotime.php
- Carbon dokumentation: https://carbon.nesbot.com/