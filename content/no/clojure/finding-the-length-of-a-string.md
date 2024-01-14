---
title:    "Clojure: Å finne lengden av en streng"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å finne lengden på en streng (string) er et grunnleggende konsept innenfor programmering. Det kan komme til nytte når du skal verifisere brukerinput, eller når du skal arbeide med tekstbaserte data. I denne bloggposten skal vi se nærmere på hvordan du kan gjøre dette i Clojure.

## Hvordan

For å finne lengden på en streng (string) i Clojure kan du bruke funksjonen `count`. Denne funksjonen tar inn en streng som parameter og returnerer antall tegn i strengen.

```Clojure
(count "Hei, verden!")
```

Dette vil gi oss følgende output: `13`

En annen måte å finne lengden på en streng på er ved å bruke funksjonen `(.length str)` fra Java Interop biblioteket. Dette kan være nyttig hvis du trenger å arbeide med Java objekter i Clojure.

```Clojure
(.length "Hello, world!")
```

Dette vil gi oss samme output: `13`

## Dykk dypere

Når du bruker `count` funksjonen, vil den gå gjennom hver enkelt karakter i strengen og telle dem opp. Dette betyr at den vil telle både bokstaver, tall og spesialtegn som én tegn. Hvis du derimot bruker `(.length str)` funksjonen, vil den basere lengden på antall bytes i strengen. Dette kan være relevant hvis du arbeider med tekst som inneholder ikke-ascii-tegn.

Selv om det er enkelt å finne lengden på en enkelt streng, kan det være mer komplekse når du skal finne lengden på en liste av strenger eller en streng som inneholder understrenger. I disse tilfellene kan du bruke funksjonen `map` sammen med `count` for å telle lengden på hver streng i listen, eller bruke `apply` sammen med `str` for å slå sammen understrengene før du teller lengden.

## Se også

- Offisiell dokumentasjon for `count` funksjonen: [https://clojuredocs.org/clojure.core/count](https://clojuredocs.org/clojure.core/count)
- Offisiell dokumentasjon for Java Interop biblioteket: [https://clojure.github.io/clojure/clojure.java.api.html](https://clojure.github.io/clojure/clojure.java.api.html)