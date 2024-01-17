---
title:                "Sammenstilling av strenger"
html_title:           "Elm: Sammenstilling av strenger"
simple_title:         "Sammenstilling av strenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å konkatenerer strenger er når man "sammenføyer" flere strenger til en enkel streng. Dette er en vanlig oppgave for programmerere når de trenger å kombinere tekst eller variabler for å lage en output.

# Hvordan:
I Elm, bruker vi operatøren "++" for å konkatenerer strenger. La oss se på et enkelt eksempel:
```Elm 
navn = "John"
melding = "Hei, mitt navn er " ++ navn
```
Dette vil gi oss en output av "Hei, mitt navn er John".

Vi kan også kombinere flere strenger ved å konkatenerer dem sammen:
```Elm
adjektiv = "kul"
emne = "Elm programmering"
utfall = "Å lære seg " ++ adjektiv ++ " " ++ emne ++ " er gøy!"
```
Dette vil gi oss en output av "Å lære seg kul Elm programmering er gøy!"

# Dypdykk:
Konkatenering av strenger er en vanlig operasjon i de fleste programmeringsspråk og har blitt brukt i mange år. Alternativt kan man også bruke funksjoner som "concat" eller "join" for å kombinere strenger. I Elm blir "++" operatøren oversatt til funksjonen "append" og fungerer på samme måte som "++".

# Se også:
Du kan lese mer om konkatenering og andre nyttige operatører i Elm på deres offisielle dokumentasjonsside: https://guide.elm-lang.org/core_language.html