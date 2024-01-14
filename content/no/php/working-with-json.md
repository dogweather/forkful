---
title:                "PHP: Å Jobbe med json"
simple_title:         "Å Jobbe med json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

JSON, som står for JavaScript Object Notation, er et populært filformat for å utveksle data mellom forskjellige programmeringsspråk. Det er enkelt å lese og skrive, og har blitt standarden for å lagre og overføre data på nettet. Å kunne jobbe med JSON er derfor en viktig ferdighet for enhver utvikler som ønsker å utveksle data på en enkel og effektiv måte.

## Hvordan

For å kunne jobbe med JSON i PHP, må du først konvertere dataen din til en JSON-streng. Dette kan gjøres ved hjelp av funksjonen `json_encode()`, som tar inn et array eller et objekt som parameter og returnerer en JSON-streng. Du kan også bruke `json_decode()`-funksjonen for å konvertere en JSON-streng tilbake til et array eller et objekt.

La oss si at vi har et array med informasjon om en person, og vi ønsker å konvertere det til en JSON-streng:

```PHP
$person = array(
    'navn' => 'Jens',
    'alder' => 30,
    'hobbyer' => array('fotball', 'musikk', 'reise')
);

$jsonString = json_encode($person);

echo $jsonString;
```

Dette vil gi oss følgende output:

```JSON
{"navn":"Jens","alder":30,"hobbyer":["fotball","musikk","reise"]}
```

Som du kan se, blir arrayet konvertert til et JSON-format. Du kan også legge til en tredje parameter i `json_encode()`-funksjonen for å få en mer lesbar output ved å sette inn `JSON_PRETTY_PRINT` som parameter.

Når du ønsker å konvertere en JSON-streng tilbake til et array, kan du bruke følgende kode:

```PHP
$jsonString = '{"navn":"Jens","alder":30,"hobbyer":["fotball","musikk","reise"]}';

$person = json_decode($jsonString, true);

print_r($person);
```

Dette vil gi oss følgende output:

```
Array
(
    [navn] => Jens
    [alder] => 30
    [hobbyer] => Array
        (
            [0] => fotball
            [1] => musikk
            [2] => reise
        )

)
```

## Deep Dive

Når du jobber med JSON, er det viktig å være oppmerksom på forskjellige typer data som kan være inkludert i en JSON-streng. Dette inkluderer tall, strenger, bolleanvisninger (true eller false), null og objekter. Du kan også inkludere flere lag med array eller objekter i en JSON-streng.

Det er også viktig å være oppmerksom på at JSON er et tekstbasert format, og derfor ikke er optimalt for å lagre store mengder data. I slike tilfeller kan det være bedre å bruke et annet filformat som er mer effektivt for å håndtere store datamengder.

En annen viktig ting å merke seg er at JSON ikke støtter kommentarer. Dette betyr at du ikke kan inkludere kommentarer i en JSON-streng, da dette vil føre til en feil.

## Se også

- [PHP: json_encode - Manual](https://www.php.net/manual/en/function.json-encode.php)
- [PHP: json_decode - Manual](https://www.php.net/manual/en/function.json-decode.php)
- [JSON.org](https://www.json.org/)