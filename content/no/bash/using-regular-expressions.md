---
title:                "Å bruke regulære uttrykk"
html_title:           "Bash: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hva og Hvorfor?
Bruk av regulære uttrykk er en vanlig praksis blant programmører når du ønsker å søke gjennom og behandle store mengder tekst på en effektiv måte. Ved hjelp av regulære uttrykk kan du søke etter mønstre i tekst og dermed spare tid og krefter på manuelt å lete gjennom dokumenter eller kode.

# Hvordan:
En enkel måte å bruke regulære uttrykk i Bash er ved å bruke kommandoen ```grep```. For eksempel kan du søke etter alle linjer i en tekstfil som inneholder ordet "programmering" ved å kjøre kommandoen ```grep "programmering" filnavn```. Resultatet vil være en liste over alle linjer som inneholder ordet "programmering".
En annen nyttig funksjon er å bruke spesielle symboler som representerer forskjellige karakterer. For eksempel kan ```^``` symbolisere starten av en linje, og ```$``` symbolisere slutten av en linje. Dette kan være nyttig når du ønsker å finne og erstatte spesifikke deler av tekst.

# Dypdykk:
Regulære uttrykk har blitt brukt i programmering siden 1950-tallet og er fortsatt en av de mest effektive verktøyene for tekstbehandling. Det finnes også andre verktøy og programmeringsspråk som støtter bruk av regulære uttrykk, som for eksempel Perl og Python. Implementasjonen av regulære uttrykk i Bash er basert på POSIX standarden, som er et sett med standarder for UNIX-lignende operativsystemer.

# Se også:
- [Bash dokumentasjon](https://www.gnu.org/software/bash/manual/bash.html#Introduction-to-Regular-Expression)
- [Regulære uttrykk-kurs på Codecademy](https://www.codecademy.com/learn/learn-regular-expressions)
- [Introduction to Regular Expressions (video)](https://www.youtube.com/watch?v=SAoTxmaax9k)