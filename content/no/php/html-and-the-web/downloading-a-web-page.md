---
title:                "Nedlasting av en nettside"
aliases:
- /no/php/downloading-a-web-page/
date:                  2024-01-20T17:44:40.139740-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å laste ned en nettside betyr å hente ned innholdet fra en URL til din lokale enhet. Programmerere gjør det for å analysere innholdet, automatisere datainnsamling eller sjekke tilgjengelighet.

## Slik gjør du:
PHP gjør nedlasting av nettsider enkelt med `file_get_contents()` og cURL-biblioteket. Her er grunnleggende eksempler:

```PHP
<?php
// Enkel nedlasting av nettside med file_get_contents
$webpageContent = file_get_contents('https://www.example.com');
echo $webpageContent;
?>
```

Output vil være HTML-koden til nettstedet, klar til å bli behandlet.

Bruk av cURL for mer kontroll:
```PHP
<?php
// Initialiserer cURL session
$ch = curl_init('https://www.example.com');

// Returtransfer som streng
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

// Utfører cURL session
$result = curl_exec($ch);

// Lukker cURL session
curl_close($ch);

// Skriver ut resultatet
echo $result;
?>
```
Her er `$result` også nettsidens HTML, som med `file_get_contents`.

## Dypdykk
Før PHP, måtte nedlasting av websider gjøres manuelt eller med spesialiserte verktøy. PHP introduserte funksjoner som `file_get_contents` og cURL-biblioteket, som tilbyr en mer strømlinjeformet tilnærming. Alternativene til PHP inkluderer kommandolinjeverktøy som `wget` og `curl`.

I praksis er cURL mer fleksibelt enn `file_get_contents`, som det lar deg håndtere HTTP-headere, POST-data og cookies. Implementeringsdetaljer er viktige; for eksempel, når man håndterer HTTPS kan det hende du må sette ytterligere cURL-opsjoner for SSL-sertifikatverifisering.

## Se Også
- PHP dokumentasjon for `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- PHP dokumentasjon for cURL: https://www.php.net/manual/en/book.curl.php
- Tutorials om hvordan du bruker cURL i PHP: https://www.php.net/manual/en/curl.examples-basic.php
