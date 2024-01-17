---
title:                "Nedlasting av en nettside"
html_title:           "PHP: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når vi snakker om å laste ned en nettside, mener vi å få tilgang til all informasjonen på en bestemt nettside og lagre den på vår datamaskin. Dette er nyttig for programmerere fordi det lar dem analysere og manipulere dataene på en mer effektiv måte.

## Hvordan:
La oss si at vi vil laste ned innholdet på en nettside med url-en "https://www.example.com". I PHP kan vi gjøre dette ved å bruke følgende kode:

```PHP 
<?php
$url = "https://www.example.com";
$content = file_get_contents($url);
echo $content
?>
```

Dette vil skrive ut hele innholdet på nettsiden, inkludert HTML-kode, bilder og annet media.

## Dypdykk:
Å laste ned nettsider har vært en viktig del av programmering siden internett ble et massemedium. I dag brukes det ofte til å automatisere oppgaver som å hente data fra flere kilder eller å lage web-skraper for å samle informasjon. Et alternativ til å bruke PHP's "file_get_contents" funksjon er å bruke et rammeverk som "cURL" for å gi mer avansert funksjonalitet og mer kontroll over nedlastingsprosessen.

## Se Også:
- https://www.php.net/manual/en/function.file-get-contents.php
- https://curl.haxx.se/