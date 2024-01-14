---
title:                "Clojure: Konvertere en streng til små bokstaver."
simple_title:         "Konvertere en streng til små bokstaver."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

I dagens teknologiske verden er det vanlig å jobbe med store mengder tekst, og ofte kan det være nødvendig å behandle tekststrenger på en spesifikk måte. Konvertering av en tekststreng til små bokstaver er en vanlig operasjon, og det er derfor viktig å ha kunnskap om hvordan man kan gjøre dette effektivt ved hjelp av Clojure.

# Hvordan gjøre det

Det er flere måter å konvertere en tekststreng til små bokstaver i Clojure. En av de enkleste måtene er å bruke funksjonen `lower-case`, som tar en tekststreng som argument og returnerer en ny streng med alle bokstavene i små bokstaver.

```Clojure
(lower-case "HEI, VERDEN!")
```
Output: ```"hei, verden!"```

En annen måte å gjøre det på er å bruke funksjonen `map` sammen med `char-lower-case`, som gjør at hver bokstav i strengen blir omgjort til en liten bokstav. Dette kan være nyttig hvis man ønsker å konvertere flere tekststrenger samtidig.

```Clojure
(map char-lower-case ["HEI, VERDEN!" "DAGEN I DAG"])
```
Output: ```("hei, verden!" "dagen i dag")```

# Dypdykk

Det er viktig å være oppmerksom på at når man bruker `lower-case` funksjonen, vil den returnere en ny streng og ikke endre den opprinnelige strengen. Dette gjelder også når man bruker `map` og `char-lower-case` sammen. Det kan også være lurt å være forsiktig med spesielle tegn og symboler, da disse kan bli feil hvis de kommer i kontakt med `lower-case` -funksjonen.

# Se også

- [Clojure dokumentasjon om lower-case funksjonen](https://clojuredocs.org/clojure.core/lower-case)
- [En guide til Clojure datastrukturer](https://github.com/teodoran/clojure-data-structures/blob/master/README-no.md)