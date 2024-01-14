---
title:                "Rust: Fjerning av tegn som matcher et mønster"
simple_title:         "Fjerning av tegn som matcher et mønster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang kjempet med å slette flere tegn som matcher et visst mønster i din Rust-kode? Det kan være en kjedelig og tidkrevende oppgave å gjøre manuelt. Heldigvis har Rust en innebygd funksjon for å gjøre dette enklere - `retain` funksjonen. I denne bloggposten vil vi utforske hvorfor og hvordan du kan bruke denne funksjonen i dine programmer.

## Hvordan

For å bruke `retain` funksjonen, må vi først opprette en vektor av tegn som vi ønsker å filtrere.

```Rust
let mut characters = vec!['a', 'b', 'c', 'd', 'e', 'f', 'g'];
```

Vi kan da bruke `retain` funksjonen, sammen med en lambda-funksjon, for å filtrere ut de tegnene vi ikke ønsker å beholde.

```Rust
characters.retain(|&c| c != 'a' && c != 'e');
```

Dette vil resultere i at vektoren vår kun inneholder `['b', 'c', 'd', 'f', 'g']`, da vi har fjernet alle tegnene som matcher `a` og `e`. Her kan du også bytte ut tegnene med et mønster du ønsker å filtrere ut, for eksempel alle tall eller store bokstaver.

```Rust
characters.retain(|&c| c.is_numeric());
```

Dette vil resultere i at vektoren kun inneholder tall.

## Deep Dive

`Retain` funksjonen bruker en lambda-funksjon for å bestemme hvilke tegn som skal beholdes og hvilke som skal fjernes. Den tar også inn `&self`, som gjør at funksjonen selv kan endre vektoren. Dette gjør at vi unngår behovet for å returnere en ny vektor og bruker mindre minne.

En ekstra fordel med å bruke `retain` funksjonen er at den også er veldig effektiv med store datamengder, da den bare går gjennom vektoren en gang i stedet for å måtte lage en ny vektor og iterere gjennom den.

## Se også

- [Dokumentasjon for `retain` funksjonen i Rust](https://doc.rust-lang.org/std/vec/struct.Vec.html#method.retain)
- [Rust begynnergids for lambda funksjoner](https://www.rust-lang.org/learn/collections#lambda-functions)
- [Mer om filterfunksjoner i Rust](https://www.rust-lang.org/learn/collections#filter-functions)