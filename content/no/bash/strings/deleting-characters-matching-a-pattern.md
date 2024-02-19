---
aliases:
- /no/bash/deleting-characters-matching-a-pattern/
date: 2024-01-20 17:41:40.714886-07:00
description: "I Bash kan du slette tegn som matcher et m\xF8nster for \xE5 forenkle\
  \ strenger eller rense data. Dette er nyttig n\xE5r du jobber med tekstbehandling\
  \ eller\u2026"
lastmod: 2024-02-18 23:08:54.041107
model: gpt-4-1106-preview
summary: "I Bash kan du slette tegn som matcher et m\xF8nster for \xE5 forenkle strenger\
  \ eller rense data. Dette er nyttig n\xE5r du jobber med tekstbehandling eller\u2026"
title: "Slette tegn som matcher et m\xF8nster"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I Bash kan du slette tegn som matcher et mønster for å forenkle strenger eller rense data. Dette er nyttig når du jobber med tekstbehandling eller automatisering av skript.

## Hvordan:
For å slette tegn fra en streng som matcher et mønster, bruk parameterexpansjon. Her er noen eksempler:

```Bash
# Sletter alle instanser av bokstaven 'a'
streng="banan"
echo "${streng//a/}"

# Output: bnn
```

```Bash
# Sletter bare det første funnet av bokstaven 'b'
streng="blåbærbrød"
echo "${streng/b/}"

# Output: låbærbrød
```

```Bash
# Sletter alt etter (og inkludert) '@' i en e-postadresse
epost="eksempel@domene.no"
echo "${epost/*@/}"

# Output: eksempel
```

## Dykk Ned:
Sletting av tegn basert på mønstre stammer fra behovet for å håndtere og manipulere tekst på en fleksibel måte i Unix-skall. Alternativer til Bash inkluderer sed, awk og Perl som gir lignende funksjonalitet med egne syntakser. Bash bruker glob-mønstre for enkel matching, mens sed og awk tillater mer komplekse regulære uttrykk.

Implementasjonsdetaljer: Pattern matching i Bash blir utført ved hjelp av 'globbing'. Når du bruker `${streng//mønster/erstattning}`, matcher Bash 'mønster' i 'streng' og erstatter det med 'erstattning'.

```Bash
# Et mer komplekst eksempel som fjerner alt unntatt bokstaver og tall
streng="bær!@#123"
echo "${streng//[^a-zA-Z0-9]/}"

# Output: bær123
```

I eksempelet ovenfor bruker vi negasjon gjennom `[^...]` for å bevare bokstaver og tall og slette alt annet.

## Se Også:
- Bash manualen: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- RegExp Matching: https://www.regular-expressions.info/
