---
title:                "Å finne lengden på en streng"
html_title:           "Ruby: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å finne lengden til en streng er en vanlig oppgave for programmerere, og det refererer til å bestemme antall tegn inkludert mellomrom i en tekststreng. Dette er nyttig for å manipulere og analysere tekstdata.

# Hvordan:
```Ruby
string = "Hei alle sammen!"
puts string.length
```
Output: 16

```Ruby
string = "Jeg elsker programmering"
puts string.length
```
Output: 23

# Dypdykk:
- Historisk kontekst: Å finne lengden av en streng ble først introdusert i programmeringsspråk som BASIC og COBOL på 1960-tallet.
- Alternativer: En annen måte å finne lengden av en streng på er å bruke ```string.count``` metoden, som teller antall ganger et spesifikt tegn eller en sekvens av tegn vises i en streng.
- Implementeringsdetaljer: De fleste programmeringsspråk har en innebygd metode for å finne lengden av en streng, som vanligvis returnerer en numerisk verdi som tilsvarer antall tegn i strengen.

# Se også:
- [Ruby dokumentasjon for strenger] (https://www.ruby-lang.org/en/documentation/standard_library/string/)
- [W3Schools for strenger i Ruby] (https://www.w3schools.com/ruby/ruby_strings.asp)
- [En "strings" artikkel fra The Codecademy] (https://www.codecademy.com/articles/ruby-string-methods)