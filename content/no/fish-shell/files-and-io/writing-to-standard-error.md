---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:25.164157-07:00
description: "\xC5 skrive til standard feil (stderr) i Fish Shell handler om \xE5\
  \ dirigere feilmeldinger eller diagnostikk separat fra standard utdata (stdout).\
  \ Programm\xF8rer\u2026"
lastmod: '2024-03-11T00:14:14.848062-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive til standard feil (stderr) i Fish Shell handler om \xE5 dirigere\
  \ feilmeldinger eller diagnostikk separat fra standard utdata (stdout). Programm\xF8\
  rer\u2026"
title: Skriving til standardfeil
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive til standard feil (stderr) i Fish Shell handler om å dirigere feilmeldinger eller diagnostikk separat fra standard utdata (stdout). Programmører gjør dette for å sikre at feilinformasjon kan identifiseres, håndteres eller omdirigeres lett, noe som letter feilsøkings- og loggføringsprosessene.

## Hvordan:

I Fish Shell kan du skrive til stderr ved å omdirigere din utdata ved bruk av `>&2`. Her er et enkelt eksempel:

```fish
echo "Dette er en feilmelding" >&2
```

Denne kommandoen gjenlyder bare en melding til stderr istedenfor stdout. Hvis du skulle skrive et skript som utgir både vanlige meldinger og feilmeldinger, kan du gjøre noe slik:

```fish
echo "Starter prosessen"
echo "En feil oppstod" >&2
echo "Prosess fullført"
```

Eksempel på utdata hvis du kjører skriptet og omdirigerer stderr til en fil:

```
Starter prosessen
Prosess fullført
```

Feilmeldingen ville ikke dukke opp i standard utdata, men ville bli funnet i filen du omdirigerte stderr til.

I scenarier som krever mer sofistikert feilhåndtering eller loggføring, kommer ikke Fish med innebygde biblioteker som er spesifikt designet for dette. Derimot, kan du dra nytte av eksterne verktøy eller skrive funksjoner for å assistere. For eksempel, å lage en enkel loggføringsfunksjon kan se slik ut:

```fish
function log_error
    echo $argv >&2
end

log_error "Dette er en avansert feilmelding"
```

Denne funksjonen `log_error` vil ta hvilken som helst streng du gir den og skrive den til stderr. Å bruke funksjoner som dette kan bidra til å holde feilhåndteringen din ren og konsistent gjennom skriptene dine.
