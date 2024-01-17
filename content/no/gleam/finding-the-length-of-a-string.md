---
title:                "Å finne lengden av en streng"
html_title:           "Gleam: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å finne lengden på en streng er en vanlig oppgave for programmerere, spesielt når det gjelder tekstbehandling og å behandle brukerinndata. Lengden på en streng refererer til antall tegn det tar opp i minnet, og dette kan være nyttig for å vurdere og manipulere teksten på riktig måte.

# Hvordan:
Gleam tilbyr en enkel måte å finne lengden på en streng gjennom `String.length` funksjonen. Her er et eksempel på hvordan den kan brukes:

```Gleam
let string = "Hei Gleam!"  
let length = String.length(string)
```

Her vil `length` variabelen inneholde verdien 10, siden strengen "Hei Gleam!" består av totalt 10 tegn. 

# Deep Dive:
Lengden på en streng har vært et viktig aspekt av dataprogrammering siden begynnelsen. I eldre språk som C, var datatyper som `char` ofte brukt til å representere en enkelt bokstav eller tegn. Å telle lengden på en streng besto derfor av å telle antall tegn før en null-terminal ble funnet. 

I dag er dataprogrammering blitt mer avansert, og de fleste moderne språk har dedikerte funksjoner for å finne lengden på en streng. I tillegg til Gleam sin `String.length`, har andre språk som Java og Python også lignende funksjoner.

# Se også:
Her er noen ressurser for å lære mer om håndtering av strenger i Gleam:
- Offisiell dokumentasjon: https://gleam.run/documentation/standard-library/string.html#length
- Gist tutorial: https://gist.github.com/sjbrown90/b5c21a28922871cbd09be4ea381aea52