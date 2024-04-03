---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:51.152169-07:00
description: "Hvordan: \xC5 jobbe med JSON i PHP er enkelt takket v\xE6re de innebygde\
  \ funksjonene `json_encode()` og `json_decode()`. Nedenfor er eksempler som viser\
  \ hvordan\u2026"
lastmod: '2024-03-13T22:44:40.907571-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON i PHP er enkelt takket v\xE6re de innebygde funksjonene\
  \ `json_encode()` og `json_decode()`."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Å jobbe med JSON i PHP er enkelt takket være de innebygde funksjonene `json_encode()` og `json_decode()`. Nedenfor er eksempler som viser hvordan man konverterer et PHP-array til en JSON-streng, og omvendt:

### Enkoding av et PHP-array til en JSON-streng
```php
// Definerer et assosiativt array
$data = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john.doe@example.com"
];

// Konverterer PHP-array til en JSON-streng
$jsonString = json_encode($data);

// Skriver ut JSON-strengen
echo $jsonString;
```
**Eksempel på utskrift:**
```json
{"name":"John Doe","age":30,"email":"john.doe@example.com"}
```

### Dekoding av en JSON-streng til et PHP-array
```php
// JSON-streng
$jsonString = '{"name":"John Doe","age":30,"email":"john.doe@example.com"}';

// Konverterer JSON-strengen til et PHP-array
$data = json_decode($jsonString, true);

// Skriver ut PHP-array
print_r($data);
```
**Eksempel på utskrift:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```

### Arbeid med et tredjepartsbibliotek: GuzzleHttp
For komplekse JSON- og webforespørselshåndteringer er et populært PHP-bibliotek GuzzleHttp. Det forenkler HTTP-forespørsler og jobber enkelt med JSON-data.

**Installasjon via Composer:**
```
composer require guzzlehttp/guzzle
```

**Eksempel på forespørsel:**
```php
require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client();

// Sender en forespørsel til en API som returnerer JSON
$response = $client->request('GET', 'https://api.example.com/data', [
    'headers' => [
        'Accept' => 'application/json',
    ],
]);

// Dekoder JSON-respons til et PHP-array
$data = json_decode($response->getBody(), true);

// Skriver ut dataene
print_r($data);
```

**Forutsatt at API-en returnerer lignende JSON-data:**
```
Array
(
    [name] => John Doe
    [age] => 30
    [email] => john.doe@example.com
)
```
Dette viser enkelheten ved å bruke PHP for JSON-manipulasjon, både med

native funksjoner og med robuste biblioteker som GuzzleHttp for mer komplekse oppgaver.
