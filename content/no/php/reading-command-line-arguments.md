---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Kommandolinjeargumenter er data input som brukes i terminalen når et program kjøres. Programmere bruker denne metoden for å spesifisere og kontrollere hvordan et program skal utføre en oppgave.

## Hvordan 

For å lese kommandolinjeargumenter i PHP, bruker vi ofte `$argv` og `$argc` globale variabler, som er innebygd i PHP. La oss se hvordan det fungerer:

```PHP
<?php
  // $argc gir oss antallet argumenter
  echo "Antall Argumenter: " . $argc . "\n"; 
  
  // $argv er en array som holder argumentene
  for ($i=0; $i<$argc; $i++) {
      echo "Argument #" . $i . " er " . $argv[$i]. "\n";
  }
?>
```

Når du kjører koden ovenfor fra kommandolinjen og passerer argumenter, vil output se slik ut:

```PHP
> php yourfile.php arg1 arg2 arg3
Antall Argumenter: 4
Argument #0 er yourfile.php
Argument #1 er arg1
Argument #2 er arg2
Argument #3 er arg3
```

## Dypdykk

Å kunne lese kommandolinjeargumenter er viktig for enhver programmerer som jobber med skript som er ment å kjøre på serveren eller fra terminalen. Denne funksjonaliteten er ikke unik for PHP, og finnes i de fleste høyere programmeringsspråk. 

Alternativt kan du bruke `getopt()` funksjonen i PHP til å parse kommandolinjeargumenter. Denne funksjonen gir mer fleksibilitet og tillater brukere å spesifisere kort- og langformat for parametere.

Implementasjonsdetaljer, `$argv` returnerer en array der det første elementet (index 0) alltid er navnet på scriptet som kjøres. Resten av elementene i arrayen er argumentene som er gitt på kommandolinjen.

## Se også

1. PHP Manual - CommandLine usage : [Link](https://www.php.net/manual/en/features.commandline.usage.php)
2. StackOverflow - How to get command line arguments for php script : [Link](https://stackoverflow.com/questions/1921421/get-the-shell-argc-argv-construct-in-php)
3. PHP `getopt()` Function : [Link](https://www.php.net/manual/en/function.getopt.php)