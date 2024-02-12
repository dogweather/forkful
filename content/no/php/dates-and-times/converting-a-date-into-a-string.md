---
title:                "Konvertere en dato til en streng"
aliases:
- /no/php/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:07.732561-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en dato til en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en dato til en streng i PHP betyr å endre et datoobjekt til et leselig format. Programmerere gjør det for å vise datoer på en forståelig måte for brukere eller for å formatere før lagring i databaser.

## Hvordan gjøre det:
```PHP
<?php
$date = new DateTime('2023-04-01');
$formattedDate = $date->format('Y-m-d H:i:s'); // ISO 8601 format
echo $formattedDate; // 2023-04-01 00:00:00

// På norsk med lokal tidssone
setlocale(LC_TIME, 'nb_NO');
echo strftime("%A %e %B %Y", $date->getTimestamp()); // Lørdag 1 april 2023
?>
```

## Dypdykk
Tidlig i PHPs historie brukte vi `date()`-funksjonen for å formatere tid. Så kom objektorientert tilnærming med `DateTime`-klassen, noe som ga bedre håndtering av tidssoner og mer komplekse datooperasjoner. Alternativene inkluderer `strftime()`, som er bra for lokaliserte datoformater, og den uavhengige `Carbon`-biblioteket for enda mer funksjonalitet.

Ved konvertering tar man hensyn til tidssoner og lokalitet. `DateTime`-objekter kan forholde seg til tidssoner, noe som betyr at du kan vise tidspunkt korrekt hvor enn brukeren befinner seg. For lokaliserte strengeformater, som visningsdatoer på norsk, bruker vi `setlocale()` og `strftime()`.

## Se også
- PHPs DateTime dokumentasjon: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- PHPs setlocale dokumentasjon: [php.net/manual/en/function.setlocale.php](https://www.php.net/manual/en/function.setlocale.php)
- Carbon's dokumentasjon: [carbon.nesbot.com](https://carbon.nesbot.com/docs/)
