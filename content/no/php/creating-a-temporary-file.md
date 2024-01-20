---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Produksjon av midlertidige filer i programmering er prosessen med dynamisk generering av en fil som kan lagres og slettes i løpet av en programøkt. Dette gjøres for å midlertidig lagre data, som kan være svært nyttig i prosesser som krever mellomlagring av informasjon.

## Sånn Gjør Du:

Her er et typisk eksempel på hvordan du lager en midlertidig fil i PHP:
```PHP
<?php
$tempFile = tmpfile();
fwrite($tempFile, "Hello, Norway!");
rewind($tempFile);
echo fread($tempFile, 1024);
fclose($tempFile);
?>
```
Når du kjører denne koden, vil du se "Hello, Norway!" skrevet ut på skjermen.

## Dypdykk

Historisk er bruken av midlertidige filer en gammel teknikk som har overlevd fordi det er effektivt i mange scenarioer. Imidlertid er det noen ulemper med dem, som potensialet for å fylle opp diskplass hvis de ikke slettes ordentlig.

Alternativene inkluderer bruken av minnebaserte datastrukturer, som arrayer, lister og så videre. Men disse har sin egen problemstilling med hensyn til begrensninger i minnestørrelse.

På implementeringsnivå bruker PHP interne støttebiblioteker for å håndtere midlertidige filer, som lar deg skrive, lese og til og med søke i disse filene før de slettes for godt.

## Se Også

Her er noen eksterne ressurser som kan være nyttige for å lære mer om midlertidige filer i PHP:

1. PHP Manual on Temporary Files: [Link](https://www.php.net/manual/en/function.tmpfile.php)
3. Usage and examples with tmpfile(): [Link](https://www.w3schools.com/php/func_filesystem_tmpfile.asp)