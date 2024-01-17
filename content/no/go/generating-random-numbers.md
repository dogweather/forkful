---
title:                "Generering av tilfeldige tall"
html_title:           "Go: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Generering av tilfeldige tall er en viktig del av programmering, og brukes ofte til å skape variasjon og tilfeldighet i spill, simuleringer og kryptografi. Programmerere bruker ofte tilfeldige tall for å lage unike brukernavn, generere unike koder og tilfeldige hendelser i sine programmer.

# Hvordan:
Go har en innebygd funksjon, "rand", som brukes til å generere tilfeldige tall. Denne funksjonen tar imot et heltall som argument og returnerer et tilfeldig tall mellom 0 og det spesifiserte tallet. For eksempel:

```
nummer := rand.Intn(10)
fmt.Println(nummer)
```
Dette vil generere et tilfeldig tall mellom 0 og 10 (ikke inkludert 10) og skrive det ut.

# Graving:
Generering av tilfeldige tall har vært en del av programmering siden de tidligste dager, og har blitt brukt i ulike sammenhenger som spill og simuleringer. Alternativer til Go inkluderer andre programmeringsspråk som også har innebygde funksjoner for å generere tilfeldige tall, som for eksempel Python og Java. Implementeringen av tilfeldige tall i programmeringsspråk er basert på algoritmer som bruker matematiske formeler for å produsere tall som ser tilfeldige ut.

# Se også:
For mer informasjon om tilfeldige tall i Go, kan du sjekke ut dokumentasjonen på Go's offisielle nettside (https://golang.org/pkg/math/rand/). Du kan også se på lignende ressurser og diskusjoner på stackoverflow (https://stackoverflow.com/questions/12321133/generating-random-numbers-in-golang).