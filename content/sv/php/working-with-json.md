---
title:                "Att arbeta med json"
html_title:           "PHP: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Varför arbeta med JSON i PHP? JSON (JavaScript Object Notation) är ett vanligt förekommande format för datalagring och utbyte av data mellan olika system och plattformar. Det är också ett populärt sätt att hämta data från API:er. Att kunna hantera JSON-data i PHP är därför en viktig kunskap för att kunna bygga dynamiska webbsidor och applikationer.

## Hur man arbetar med JSON i PHP

För att kunna arbeta med JSON i PHP behöver du först konvertera datan till en PHP-array. Detta kan göras på två sätt - antingen genom att använda funktionen `json_decode()` för att konvertera en JSON-sträng till en PHP-array, eller genom att använda `json_encode()` för att konvertera en PHP-array till en JSON-sträng.

Här är ett exempel på hur man konverterar en JSON-sträng till en PHP-array:

```PHP
<?php
// JSON-strängen som ska konverteras
$json = '{"name":"John","age":30,"city":"Stockholm"}';

// Konvertera JSON-strängen till en PHP-array
$array = json_decode($json, true);

// Skriv ut namnet
echo $array['name']; // John
```

Här är ett exempel på hur man konverterar en PHP-array till en JSON-sträng:

```PHP
<?php
// PHP-arrayen som ska konverteras
$array = array('name' => 'John', 'age' => 30, 'city' => 'Stockholm');

// Konvertera PHP-arrayen till en JSON-sträng
$json = json_encode($array);

// Skriv ut JSON-strängen
echo $json; // {"name":"John","age":30,"city":"Stockholm"}
```

Att arbeta med JSON i PHP är relativt enkelt och det finns många inbyggda funktioner som gör det smidigt att hantera både JSON-strängar och PHP-arrayer.

## Djupdykning

En viktig aspekt att ha i åtanke när man arbetar med JSON i PHP är att kontrollera datatypen innan man börjar konvertera. Om en felaktig datatyp skickas till en av funktionerna (`json_decode()` eller `json_encode()`) kan det leda till oväntade resultat.

En annan viktig sak att tänka på är hantering av eventuella fel. Om konverteringen av JSON till en PHP-array misslyckas, kommer funktionen `json_decode()` att returnera `null`. Detta kan leda till problem om man försöker använda datan utan att först kontrollera dess värde. Det är därför viktigt att alltid ha en kontrollstruktur för att hantera eventuella fel.

Att känna till de vanligaste funktionerna för att hantera JSON i PHP är också viktigt. Några sådana funktioner är `json_decode()`, `json_encode()`, `json_last_error()` och `json_last_error_msg()`. Genom att känna till vad dessa funktioner gör och hur de används, blir det enklare att arbeta med JSON i PHP.

## Se även

- [PHP-dokumentationen för JSON](https://www.php.net/manual/en/book.json.php)
- [JSON i JavaScript](https://www.json.org/json-sv.html)
- [JSON Formatter & Validator](https://jsonformatter.curiousconcept.com/)