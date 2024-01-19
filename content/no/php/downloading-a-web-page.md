---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Laste ned en nettside med PHP

## Hva & Hvorfor?
Å laste ned en nettside er prosessen med å hente dataene fra en server og lagre det på en lokal enhet. Programmerere gjør dette for å analysere, teste eller manipulere nettstedets innhold.

## Hvordan gjøre det:
La oss begynne med et enkelt eksempel. For å laste ned en nettside i PHP, bruker vi “cURL” biblioteket. Her er en grunnleggende kodebit:

```PHP
<?php
  $curl = curl_init();
  curl_setopt($curl, CURLOPT_URL, "http://eksempel.no");
  curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);
  $result = curl_exec($curl);
  curl_close($curl);
  
  file_put_contents("eksempel.html", $result);
?>
```
Denne koden vil laste ned HTML-innholdet fra http://eksempel.no, og lagre det i en fil som heter 'eksempel.html'.

## Dypdykk
Historisk sett har PHP, siden utgivelsen på 90-tallet, gjennomgått mange forbedringer for å støtte nettside nedlasting. Før cURL, var file_get_contents() eller fopen() mer populære. 

Alternativt til cURL, kan vi fortsatt bruke file_get_contents() som er mer fremoverkompatibel, men det gir mindre kontroll over nedlastingsprosessen: 

```PHP
<?php
  $result = file_get_contents("http://eksempel.no");
  file_put_contents("eksempel.html", $result);
?>
```
Implementeringsdetaljer om nedlasting av en nettside avhenger av mange faktorer, som serverens responstid, størrelsen på nettsiden, og inkluderer håndtering av feil og unntak som kan oppstå.

## Se også
1. [PHP: cURL - Manual](http://php.net/manual/en/book.curl.php) 
2. [PHP: file_get_contents - Manual](http://php.net/manual/en/function.file-get-contents.php) 
3. [PHP: fopen - Manual](http://php.net/manual/en/function.fopen.php) 

Dypere læring krever praksis. Fortsett å kode, og lykke til på din programmeringsreise!