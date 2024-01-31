---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
JSON, eller JavaScript Object Notation, brukes for å utveksle data mellom server og webapplikasjoner. Programmerere bruker JSON på grunn av formatets letthet og fordi det er enkelt å tolke for mennesker, samt lett å parse for maskiner.

## Hvordan:
Å jobbe med JSON i PHP er rett frem. Her's hvordan du gjør det i kode:

```PHP
<?php
// Enkod et PHP-array til en JSON-string
$data = array("navn" => "Ola", "alder" => 30, "by" => "Oslo");
$json_data = json_encode($data);
echo $json_data;
```

Output vil være en JSON-formattert string:
```
{"navn":"Ola","alder":30,"by":"Oslo"}
```

For å dekode tilbake til PHP:

```PHP
<?php
// Dekod en JSON-string til et PHP-array
$json_string = '{"navn":"Ola","alder":30,"by":"Oslo"}';
$php_array = json_decode($json_string, true);
print_r($php_array);
```

Og output blir et assosiativt array i PHP:
```
Array
(
    [navn] => Ola
    [alder] => 30
    [by] => Oslo
)
```

## Dypdykk
JSON ble opprettet rundt tidlig 2000-tallet og er basert på JavaScript-syntaxen. Alternativer til JSON inkluderer XML og YAML, men JSON har vokst i popularitet takket være dets enkelhet og støtte i mange programmeringsspråk. PHP tilbyr innebygde funksjoner som `json_encode()` og `json_decode()`, noe som gjør det lett å implementere. Når du bruker `json_decode()`, pass på å bruke det andre argumentet for å spesifisere om du vil ha et array eller et objekt tilbake.

## Se Også

- PHP Manual om JSON: https://www.php.net/manual/en/book.json.php
- JSON Offisiell Nettside: https://www.json.org/json-en.html
- W3Schools PHP JSON Tutorial: https://www.w3schools.com/php/php_json.asp
