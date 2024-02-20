---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:10.826723-07:00
description: "Een HTTP-verzoek verzenden met basisauthenticatie houdt in dat je een\
  \ gebruikersnaam en wachtwoord toevoegt om toegang te krijgen tot een resource op\
  \ een\u2026"
lastmod: 2024-02-19 22:05:09.966938
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek verzenden met basisauthenticatie houdt in dat je een gebruikersnaam\
  \ en wachtwoord toevoegt om toegang te krijgen tot een resource op een\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek verzenden met basisauthenticatie houdt in dat je een gebruikersnaam en wachtwoord toevoegt om toegang te krijgen tot een resource op een server. Programmeurs gebruiken het omdat sommige API's en webdiensten authenticatie vereisen om ervoor te zorgen dat alleen geautoriseerde gebruikers toegang hebben tot hun gegevens.

## Hoe te:

Hier is de eenvoudige manier om een HTTP-verzoek te verzenden met basisauthenticatie met cURL in PHP:

```PHP
<?php
$url = 'https://api.voorbeeld.com/data';
$username = 'jouw_gebruikersnaam';
$password = 'jouw_wachtwoord';

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

Voorbeelduitvoer:

``` 
{
  "authenticated": true,
  "data": "Enige beveiligde gegevens"
}
```

## Uitdieping

HTTP Basisauthenticatie wordt al gebruikt sinds de vroege dagen van het web. Het is niet de meest veilige optie rond (aangezien referenties worden verzonden in base64-codering, wat gemakkelijk te decoderen is), maar het is eenvoudig te implementeren voor snelle en vuile toegangscontrole.

Stel dat veiligheid een zorg is (en dat zou het moeten zijn), dan wend je je tot robuustere methoden zoals OAuth, JWT of API-sleutels. Maar, basisauthenticatie blijft deels bestaan vanwege legacy-systemen en deels voor interne systemen waar je de toegang strikt controleert.

In PHP wordt cURL veel gebruikt voor het maken van HTTP-verzoeken, maar alternatieven zoals `file_get_contents` of Guzzle (een PHP HTTP-client) bestaan. Bij het gebruik van `file_get_contents` moet een context met de geschikte header worden gemaakt:

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$response = file_get_contents($url, false, $context);

echo $response;
?>
```

Het kiezen van het juiste hulpmiddel hangt af van de behoeften van je project en het niveau van controle en functionaliteit dat je wenst.

## Zie ook

Om dieper te duiken en je kennis uit te breiden, bekijk deze:

- [cURL documentatie](https://www.php.net/manual/en/book.curl.php)
- [PHP `file_get_contents` functie](https://www.php.net/manual/en/function.file-get-contents.php)
- [HTTP-authenticatie met PHP](https://www.php.net/manual/en/features.http-auth.php)
