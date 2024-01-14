---
title:    "Gleam: Oppretting av en midlertidig fil"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer kan være en nyttig funksjon når du jobber med programmering. Det kan hjelpe deg med å lagre midlertidige data, behandle store mengder informasjon og sikre at dataene dine ikke går tapt underveis.

## Slik gjør du det

Hvis du bruker Gleam, kan du enkelt opprette en midlertidig fil ved hjelp av følgende kode:

```Gleam

File.create_temporary("/sti/til/fil", "txt")

```

Dette vil lage en midlertidig fil med et tilfeldig generert navn og en "txt" -utvidelse i den angitte stien. Du kan deretter jobbe med denne filen som du vil, og når du er ferdig, slette den med følgende kode:

```Gleam

File.delete("/sti/til/fil.txt")

```

Dette vil slette filen og frigjøre all plass den opptar.

## Dyp dykk

Du kan også endre navnet og utvidelsen til den midlertidige filen ved å bruke funksjonene "set_name" og "set_extension" på filobjektet. Du kan også spesifisere en annen midlertidig mappe for å lagre filen ved å bruke "set_directory" -funksjonen.

Sørg for å håndtere eventuelle feil som kan oppstå under opprettelsen eller slettingen av en midlertidig fil, for å unngå problemer med programmet ditt.

## Se også

- [Gleam File Modulen](https://gleam.run/mod/file/)
- [Gleam Dokumentasjon](https://gleam.run/)

Takk for at du leste! Vi håper dette hjalp deg med å lære mer om å opprette midlertidige filer i Gleam. Lykke til med kodingen!