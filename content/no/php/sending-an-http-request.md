---
title:                "PHP: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å sende en HTTP forespørsel er en av de mest grunnleggende handlingene i webskraping og webutvikling. Det lar deg hente data, sende informasjon og utføre andre handlinger på eksterne nettsider. Uten dette viktige konseptet ville det vært vanskelig å interagere med andre nettsteder og lage dynamiske og interaktive applikasjoner.

# Hvordan

For å sende en HTTP forespørsel i PHP, trenger du bare å bruke funksjonen `file_get_contents ()`. Dette vil lage en forespørsel til en bestemt URL og returnere innholdet på den siden som en streng. La oss si for eksempel at vi vil hente data fra Google sin søkemotor:

```PHP
<?php
$response = file_get_contents("https://www.google.com/search?q=web scraping");

echo $response;

// Output: Det returnerte innholdet på Google sin søkeside om web skraping
?>
```

Dette er det grunnleggende konseptet bak HTTP forespørsler i PHP. Du kan også legge til ekstra parametere i forespørselen, for eksempel å sende en POST-forespørsel eller spesifisere HTTP-hodene. Dette gir deg mer kontroll og fleksibilitet når du samhandler med et nettsted.

# Dypdykk

Når du har forstått hvordan du sender enkle HTTP forespørsler, kan du begynne å dykke dypere inn i konseptet. For eksempel kan du lære om hvordan du håndterer eventuelle feil som kan oppstå under sending av en forespørsel, og hvordan du håndterer autentisering og sikkerhet. Du kan også utforske forskjellige biblioteker og verktøy som kan hjelpe deg med å håndtere HTTP forespørsler mer effektivt.

# Se også

* [PHP offisiell dokumentasjon for file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
* [En omfattende guide til PHP HTTP forespørsler](https://www.taniarascia.com/how-to-use-php-curl-and-http-requests/) (på engelsk)
* [En enkel guide for å sende HTTP forespørsler med PHP](https://hackernoon.com/how-to-send-http-requests-using-php-easily-773f95292aaf) (på engelsk)