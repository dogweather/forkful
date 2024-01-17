---
title:                "Å jobbe med json"
html_title:           "PHP: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON står for JavaScript Object Notation og er et vanlig format for å lagre og utveksle data mellom forskjellige programmeringsspråk. Programmere bruker JSON fordi det er enkelt å lese og skrive, og det er også et standardisert format som mange programmer og API-er benytter seg av.

## Slik gjør du det:
Her er et eksempel på hvordan du kan jobbe med JSON i PHP:

```PHP
// Opprett et JSON-objekt med data
$json = '{"navn": "Ola", "alder": 28, "jobb": "webutvikler"}';

// Konverter JSON til et PHP-assosiativt array
$arr = json_decode($json, true);

// Hent ut og skrive ut informasjon fra arrayet
echo "Navn: " . $arr['navn'];
echo "Alder: " . $arr['alder'];
echo "Jobb: " . $arr['jobb'];
```

Dette vil gi følgende output:

```
Navn: Ola
Alder: 28
Jobb: webutvikler
```

## Dypdykk:
JSON ble utviklet på 90-tallet som en enkel måte å representere data på. Det er inspirert av syntaksen til JavaScript, men det kan brukes med mange andre programmeringsspråk også. Alternativer til JSON inkluderer for eksempel XML og YAML. For å jobbe med JSON i PHP, er det viktig å være kjent med funksjonene `json_encode()` og `json_decode()`, samt å validere og sikre mot uønsket JSON-data.

## Se også:
- [Dokumentasjon for PHP sin `json_encode()` funksjon](https://www.php.net/manual/en/function.json-encode.php)
- [Dokumentasjon for PHP sin `json_decode()` funksjon](https://www.php.net/manual/en/function.json-decode.php)
- [Eksempler på bruk av JSON i PHP](https://www.w3schools.com/php/php_ajax_php.asp)