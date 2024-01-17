---
title:                "Sammenslåing av strenger"
html_title:           "Bash: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor
 
 Strenger, også kjent som tekst, er en vanlig type data som programmerere jobber med. Noen ganger må vi kombinere eller legge sammen flere strenger for å skape en ny og lengre streng. Dette kalles å konkatenerere strenger, og det er en nyttig teknikk for å bygge dynamiske og tilpassede tekster. 
 
 ## Hvordan:
 
 For å konkatenerere strenger i Bash, kan vi bruke operatøren ```+```. La oss se på et enkelt eksempel: 
 
 ``` 
 string1="Hei,"
 string2="verden!"
 echo $string1$string2
 ```
Output: Hei, verden!
 
Som du kan se, kombinerer Bash operatorene sammen to strenger og skaper en ny streng. Vi kan også bruke variabler i konkateneringen, og til og med legge til ord og symboler. La oss se på et eksempel som demonstrerer dette:
 
```
name="John"
echo "Hei, $name! Velkommen til å programmere i Bash!"
```
Output: Hei, John! Velkommen til å programmere i Bash!
 
## Dykk ned:
 
Konkaterering av strenger er ikke en ny konsept, og den er tilgjengelig i mange programmeringsspråk, inkludert Bash. Men i Bash er ```+``` operatøren ikke alltid brukt. I stedet, i mange tilfeller, brukes ```${string1}${string2}``` syntaksen. Det er også verdt å nevne at du kan konkatenerere så mange strenger som du vil i en enkelt kommando. Det er også nyttig å vite at det finnes alternativer i Bash, for eksempel ```printf``` kommando, for å generere og formatere tekster på en mer effektiv måte. 
 
## Se også:
 
For mer informasjon om å konkatenerere strenger i Bash, sjekk ut denne ressursen: [https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html]. Du kan også finne nyttige tips og triks i Bash-dokumentasjonen og på online programmeringsforum. Lykke til!