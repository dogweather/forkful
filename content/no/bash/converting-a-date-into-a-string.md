---
title:                "Bash: Konvertering av dato til streng"
simple_title:         "Konvertering av dato til streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
Nå som stadig flere bedrifter og organisasjoner tar i bruk Bash-programmering, er det viktig å kunne mestre ulike aspekter av dette programmeringsspråket. En av de vanligste utfordringene i Bash er å konvertere en dato til en streng. Dette kan være nødvendig i ulike situasjoner, for eksempel når man skal generere rapporter eller manipulere filnavn. Derfor er det nyttig å vite hvordan man enkelt kan konvertere en dato til en streng i Bash.

# Hvordan
Bash tilbyr flere ulike metoder for å konvertere en dato til en streng, men den enkleste og mest pålitelige måten er å bruke "date" kommandoen. Denne kommandoen har en rekke formateringsalternativer som gjør det enkelt å konvertere en dato til en ønsket streng.

For å konvertere dagens dato til en streng, kan du bruke følgende kode:

```Bash
today=$(date +'%d-%m-%Y')
echo $today
```
Dette vil resultere i en output som ser slik ut:

```Bash
25-11-2020
```

På samme måte kan du også tilpasse formatet etter dine behov. For eksempel, hvis du ønsker å legge til navnet på måneden og året, kan du bruke følgende kode:

```Bash
current_date=$(date +'%d %B %Y')
echo $current_date
```
Dette gir en output som ser slik ut:

```Bash
25 September 2020
```

# Dypeste dykk
Det finnes flere ulike formateringsalternativer for "date" kommandoen, og disse kan brukes til å tilpasse utseendet på den konverterte datoen. Noen av de vanligste alternativene er:

- %A: Navnet på dagen
- %B: Navnet på måneden
- %d: Dagen i måneden (med ledende null)
- %m: Måneden (med ledende null)
- %Y: Året (med fire siffer)
- %y: Året (med to siffer)
- %I: Timen (12-timers format)
- %H: Timen (24-timers format)
- %M: Minutten
- %S: Sekunden

I tillegg til disse, kan du også legge til spesifikke "locales" for å konvertere datoer på andre språk. Du kan lese mer om dette i Bash dokumentasjonen.

# Se også
- Bash dokumentasjon: https://www.gnu.org/software/bash/
- Norsk Bash forum: https://www.diskusjon.no/forum/100-bash-programmering/