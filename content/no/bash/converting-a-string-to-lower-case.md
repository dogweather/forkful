---
title:                "Konverterer en streng til små bokstaver"
html_title:           "Bash: Konverterer en streng til små bokstaver"
simple_title:         "Konverterer en streng til små bokstaver"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av en streng til små bokstaver er en vanlig operasjon i programmering. Dette betyr rett og slett å endre alle store bokstaver i en streng til små bokstaver. Dette gjøres ofte for å sammenligne to strenger med ulik casing, eller for å få en mer enhetlig formatering.

## Hvordan:

```Bash
# Bruk kommandoen "tr" for å konvertere en streng til små bokstaver
# Eksempel:
echo "DIg It??" | tr 'A-Z' 'a-z'

# Output:
dig it??
```

## Dypdykk:

Historisk sett har casing av tekst vært en utfordring i programmering. Tidligere var det vanlig å bruke store bokstaver for variabelnavn og funksjonsnavn, men dette kunne føre til forvirring og feil siden casing var viktig. Derfor har mange programmeringsspråk nå støtte for å konvertere en streng til små bokstaver for å unngå slike problemer.

Det finnes også alternative måter å konvertere en streng til små bokstaver på, som for eksempel å bruke innebygde funksjoner i programmeringsspråket eller å bruke regex-uttrykk. Implementasjonen av denne operasjonen varierer også fra språk til språk, så det kan være lurt å se på dokumentasjonen til det aktuelle språket for nøyaktig syntaks og funksjonalitet.

## Se også:

- [Dokumentasjon for "tr" kommandoen i Bash](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Diskusjon om hvorfor casing er viktig i programmering](https://stackoverflow.com/questions/777416/why-should-javas-string-to-lowercase-method-not-be-used-when-converting-from-fo)