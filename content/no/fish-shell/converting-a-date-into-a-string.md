---
title:                "Konvertere en dato til en streng"
date:                  2024-01-20T17:36:25.913792-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en dato til en streng"

category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Å konvertere en dato til en streng betyr å endre dens format fra et daterepresentasjonssystem til en tekstrepresentasjon. Programmerere gjør dette for å forenkle visningen og lagringen av datoer, og for å muliggjøre sammenligninger og sortering etter dato.

## How to:

Fish Shell gjør det lett å formatere datoer. Bruk `date` kommandoen med ønskede opsjoner.

```Fish Shell
# Viser dagens dato i formatet YYYY-MM-DD
set today (date "+%Y-%m-%d")
echo $today
```
```Shell
2023-04-01
```

```Fish Shell
# Konverterer en spesifikk dato til en mer lesbar form
set birthday "1990-10-30"
set formatted_birthday (date -u -j -f "%Y-%m-%d" $birthday "+%A, %d %B %Y")
echo $formatted_birthday
```
```Shell
Tuesday, 30 October 1990
```

## Deep Dive

I historisk sammenheng, før digitale datamaskiner, ble datoer og tider oftest skrevet for hånd. Datamaskinrevolusjonen krevde et standardformat for effektiv sortering og lagring. Standarder som ISO 8601 kom for å lettvint identifisere datoer.

Fish Shell har ikke innebygd støtte for datohåndtering slik noen andre skall har, så den bruker eksterne kommandoer som `date`. `date` er fleksibel og kraftfull, med mulighet for å spesifisere både inndata- og utdataformat ved hjelp av formatstrenger.

Alternativt kan du installere et Fish-plugin, for eksempel `fish-datetime`, for mer direkte datohåndtering.

Implementasjonsdetaljer for `date` avhenger av operativsystemet. På Linux og macOS er `date` kommandoene ganske like, men med noen små forskjeller i opsjoner og syntaks.

## See Also

- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils `date` info](https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation)
- [ISO 8601 Date and Time Format](https://www.iso.org/iso-8601-date-and-time-format.html)
