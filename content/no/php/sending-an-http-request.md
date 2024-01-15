---
title:                "Send en http-forespørsel"
html_title:           "PHP: Send en http-forespørsel"
simple_title:         "Send en http-forespørsel"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

[//]: #

# Hvorfor

Å sende en HTTP forespørsel er en essensiell del av webutvikling. Dette lar deg kommunisere med et annet program eller en server for å hente eller sende data. Dette er spesielt nyttig for å integrere tjenester og data mellom forskjellige applikasjoner.

# Hvordan

For å sende en HTTP forespørsel i PHP, bruker du funksjonen `file_get_contents()`. Denne funksjonen tar URL'en som argument og returnerer innholdet av den spesifiserte URL'en. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```PHP
<?php
$url = "https://www.example.com";
$content = file_get_contents($url);
echo $content;
?>
```
Outputen vil være HTML-innholdet til www.example.com.

Hvis du ønsker å sende en POST forespørsel i stedet, kan du bruke funksjonen `file_put_contents()`. Denne funksjonen tar også URL'en som argument, men lar deg også spesifisere innholdet som skal sendes. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```PHP
<?php
$url = "https://www.example.com";
$data = array(
    'username' => 'Brukernavn',
    'password' => 'Passord'
);
$options = array(
    'http' => array(
        'method' => 'POST',
        'content' => http_build_query($data)
    )
);
$context = stream_context_create($options);
$result = file_get_contents($url, false, $context);
echo $result;
?>
```

Dette eksempelet sender en POST forespørsel til www.example.com med brukernavn og passord, og returnerer resultatet av forespørselen.

# Dypdykk

I tillegg til å bruke `file_get_contents()` og `file_put_contents()` funksjonene, kan du også bruke PHPs innebygde cURL bibliotek for å sende HTTP forespørsler. Dette gir deg mer kontroll over forespørselen og lar deg blant annet spesifisere forskjellige typer forespørsler (GET, POST, PUT, DELETE) og tilpasse forespørselens header.

Her er et eksempel på hvordan du kan bruke cURL til å sende en GET forespørsel:

```PHP
<?php
$url = "https://www.example.com";
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$result = curl_exec($ch);
curl_close($ch);
echo $result;
?>
```

Dette eksempelet oppretter en cURL-sesjon, setter URL'en som skal åpnes, og returnerer innholdet i stedet for å skrive det til skjermen. Deretter stenges sesjonen og innholdet skrives til skjermen.

# Se også

- [PHP Manual: Funksjonen file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [PHP Manual: Funksjonen file_put_contents()](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP Manual: cURL biblioteket](https://www.php.net/manual/en/book.curl.php)