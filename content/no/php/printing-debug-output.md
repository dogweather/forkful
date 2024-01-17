---
title:                "Utskrift av feilsøkingsmeldinger"
html_title:           "PHP: Utskrift av feilsøkingsmeldinger"
simple_title:         "Utskrift av feilsøkingsmeldinger"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Printing debug output er en måte for programmerere å få informasjon om hva som skjer i koden deres. Det er nyttig for å finne feil og forbedre ytelsen til koden.

## Hvordan:
Enkelt og greit, du kan bruke funksjonen `echo` for å skrive ut verdier eller variabler til skjermen. For å legge til kontekst, kan du også bruke `print_r` for å skrive ut hele arrayer eller objekter. Her er et eksempel:

```
<?php
$navn = "Maria";
$alder = 27;
echo "Hei, mitt navn er " . $navn . " og jeg er " . $alder . " år gammel.";
?>
```

Dette vil skrive ut følgende linje: `Hei, mitt navn er Maria og jeg er 27 år gammel.`

## Dykk Ned:
Å skrive ut debug output har vært en viktig del av programvareutvikling siden begynnelsen. Det hjelper utviklere å finne feil og forbedre koden sin. Det finnes også flere alternativer til `echo` og `print_r`, som `var_dump` og `debug_print_backtrace`. Disse funksjonene gir mer detaljert informasjon, men kan også være mer vanskelige å lese og forstå. I eldre versjoner av PHP, var det vanlig å bruke `print` funksjonen, men den har blitt erstattet av `echo`. For å unngå å skrive ut debug output til den vanlige brukeren av en nettside, kan du bruke `error_log` funksjonen til å sende informasjonen til en loggfil i stedet.

## Se også:
[PHP manual for printing output](https://www.php.net/manual/en/function.echo.php)