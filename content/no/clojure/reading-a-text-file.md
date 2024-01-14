---
title:    "Clojure: Lese en tekstfil"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Hvorfor

Lesing av tekster er en grunnleggende funksjon i programmering som tillater oss å manipulere og analysere store mengder data. Å kunne lese fra en tekstfil er nyttig for å kunne håndtere tekstbaserte filformater som CSV og JSON, samt å importere eksterne data til våre Clojure-prosjekter.

# Hvordan

For å lese en tekstfil i Clojure, må vi først åpne filen ved hjelp av `with-open` funksjonen og angi filstien som en argument. Deretter bruker vi `reader` funksjonen for å lese filinnholdet.

```
( with-open [reader (reader "/sti/til/filen.txt")]
    ( doseq [linje (line-seq reader)]
        (println linje)))

```

I denne koden bruker vi `with-open` funksjonen for å sørge for at filen blir lukket etter at vi er ferdige med å lese den. Deretter bruker vi `doseq` funksjonen til å gå gjennom hver linje i filen og printe den ut ved hjelp av `println` funksjonen.

## Utdata

Med koden ovenfor vil vi få ut følgende resultat:

```
Dette er en tekstfil.
Med noen linjer av tekst.
Vi kan lese linje for linje.
Kan også gjøre manipulasjoner med dataen.
```

# Dypdykk

I Clojure er tekstfiler representert som sekvenser av tegn, og `reader` funksjonen returnerer en sekvens av tegn for hver linje i filen. Derfor kan vi bruke alle de nyttige funksjonene som brukes på sekvenser, som for eksempel `doseq` og `map`.

Vi kan også spesifisere tegnkodingen til tekstfilen ved å legge til en ekstra parameter i `reader` funksjonen. Som standard vil Clojure bruke UTF-8 tegnkoding, men dersom filen er i en annen tegnkoding kan dette spesifiseres for å sikre at dataen blir riktig behandlet.

# Se også

- [Lesing og skriving av filer i Clojure](https://clojure.org/guides/io)
- [Clojure sekvenser](https://clojure.org/reference/sequences)
- [Clojure funksjoner](https://clojure.org/reference/functions)