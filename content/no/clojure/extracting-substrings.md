---
title:    "Clojure: Utvinne understrenger"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med tekstbehandling, kan det være nyttig å kunne hente ut deler av en streng. Dette kalles å ekstrahere substringer, og det kan være en nyttig funksjon å ha i verktøykassen din når du programmerer i Clojure. I denne bloggposten vil jeg vise deg hvordan du kan ekstrahere substringer enkelt og effektivt.

## Hvordan

For å ekstrahere en substring i Clojure, bruker vi funksjonen `subs`, som tar tre argumenter: strengen du vil ekstrahere fra, startindeksen til substringen og eventuelt sluttpindeksen (eksklusiv). Her er et enkelt eksempel:

```Clojure
(def s "Hei, dette er en tekststreng!")
(subs s 4 9)
```

Dette vil gi følgende output:

```
"dette"
```

Vi kan også bruke negative indekser, som gjør at vi teller bakfra. For eksempel vil `subs s -5` gi oss de siste fem tegnene i strengen. Vi kan også bruke `subs` til å ekstrahere fra en gitt indeks til slutten av strengen ved å utelate sluttpindeksen:

```Clojure
(subs s 7)
```

Dette vil gi følgende output:

```
"dette er en tekststreng!"
```

Vi kan også bruke `subs` med andre datatyper enn strenger. For eksempel kan vi ekstrahere fra en liste ved å konvertere den til en streng først:

```Clojure
(def lst [1 2 3 4 5])
(subs (str lst) 1 4)
```

Dette vil gi følgende output:

```
"234"
```

Hvis du vil ha en mer nøyaktig ekstrahering, kan du bruke funksjonen `subseq`, som tar de samme argumentene som `subs`, men returnerer en sekvens i stedet for en streng. Dette kan være nyttig hvis du vil jobbe med substringer på en mer fleksibel måte.

## Deep Dive

Når du bruker `subs` eller `subseq`, er det viktig å være klar over at disse funksjonene tar inn indekser i Unicode-format, ikke enkelttegn. Dette betyr at hvis du har en flerspråklig streng, kan enkelte tegn telles som mer enn én indeks, og dette kan påvirke resultatet ditt. Det er også viktig å være nøye med om du bruker inclusive eller exclusive indekser, for å få ønsket resultat.

## Se Også

- [ClojureDocs - subs](https://clojuredocs.org/clojure.core/subs)
- [ClojureDocs - subseq](https://clojuredocs.org/clojure.core/subseq)
- [Unicode character encoding](https://www.unicode.org/versions/Unicode11.0.0/)