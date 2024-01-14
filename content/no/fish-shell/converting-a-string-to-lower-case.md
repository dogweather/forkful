---
title:    "Fish Shell: Konvertering av en streng til små bokstaver"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Hvorfor

Mange programmerere ønsker å konvertere en streng til små bokstaver av flere grunner. Det kan for eksempel være for å sammenligne tekst uten å tenke på store og små bokstaver, eller for å følge gitte konvensjoner i programmeringsspråket de bruker. Uansett grunn, kan Fish Shell gjøre dette enkelt og effektivt.

##Hvordan gjøre det

Fish Shell har en innebygd funksjon som heter `lowercase` som gjør akkurat det navnet antyder - den konverterer en streng til små bokstaver. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Fish Shell
set my_str "FISH SHELL"
echo "$my_str"
echo (lowercase "$my_str")
```

Dette vil gi følgende utoutput:

```Fish Shell
FISH SHELL
fish shell
```

Som du kan se, konverterte `lowercase`-funksjonen strengen `FISH SHELL` til små bokstaver. Det er viktig å merke seg at denne funksjonen ikke endrer den originale variabelen, men heller returnerer en ny verdi.

Hvis du vil konvertere en hel fil til små bokstaver, kan du bruke `tr`-kommandoen i kombinasjon med `lowercase`-funksjonen. For eksempel:

```Fish Shell
cat my_file.txt | tr '[:upper:]' '[:lower:]'
```

Dette vil skrive ut innholdet av filen `my_file.txt` med alle bokstaver konvertert til små bokstaver.

##Dykk dypere

Hvis du er interessert i å lære mer om hvordan Fish Shell konverterer en streng til små bokstaver, kan du ta en titt på kildekoden. Du vil se at selve funksjonen er ganske enkel og kun bruker et enkelt `for`-løkke for å gjøre konverteringen. Dette er en flott måte å dykke dypere inn i Fish Shells funksjonalitet og forstå hvordan ting fungerer under overflaten.

##Se også

- Fish Shell sin offisielle dokumentasjon om `lowercase`-funksjonen: https://fishshell.com/docs/current/cmds/lowercase.html
- En bloggpost om å konvertere store og små bokstaver med `tr`-kommandoen: https://www.howtogeek.com/196653/how-to-use-the-tr-command-on-linux/
- En artikkel om grunnleggende bruk av Fish Shell: https://medium.com/@michaelburri/fish-shell-a-beginners-guide-79a9b0df1c3e