---
title:                "Konvertere en dato til en streng"
html_title:           "Clojure: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Konvertering av datoer til strenger betyr å ta en dato og konvertere den til en tekststrekning, som for eksempel "20. mars 2020". Dette kan være nyttig for mange programmeringsspråk, da det gjør det enklere å håndtere og vise datoer i ulike formater.

Hvorfor gjør programmerere dette? Det kan være mange grunner, alt fra å vise datoer i brukergrensesnitt til å lagre dem i en database. Uansett hva målet er, er å kunne konvertere en dato til en streng en nyttig ferdighet for alle som jobber med datoer i sin kode.

## Hvordan:
Datoer kan konverteres til strenger på ulike måter i Clojure. Her er to eksempler:

```Clojure
(->> (Instant/now)
  (.toString))
;; "2020-03-20T10:20:45.523Z"

(->> (LocalDate/now)
  (.format DateTimeFormatter/ISO_LOCAL_DATE))
;; "2020-03-20"
```

I det første eksemplet bruker vi `Instant` - en klasse som representerer en tidspunkt på tidslinjen i UTC. Ved å kalle `.toString` på `Instant/now` , får vi en streng som viser den nåværende datoen og tiden i UTC-format.

I det andre eksemplet bruker vi `LocalDate` - en klasse som representerer en dato uten tidskomponent. Vi formaterer datoen ved å kalle `.format` på dagens dato med `DateTimeFormatter/ISO_LOCAL_DATE` som definerer hvordan datoen skal vises (i dette tilfellet ISO standard).

## Dypdykk:
Historisk sett var manipulering av datoer utfordrende for mange programmeringsspråk. Dette skyldes hovedsakelig at datoer har vært representert på ulike måter, som for eksempel dager siden fra en bestemt dato eller som antall millisekunder siden en spesifikk dato og tid.

I dag har de fleste språk implementert ulike klasser og metoder for å håndtere datoer på en mer intuitiv måte. I Clojure finnes det flere nyttige klasser og funksjoner for å håndtere datoer, inkludert de som ble nevnt i eksemplene ovenfor.

Alternativene til å konvertere datoer til strenger i Clojure inkluderer å bruke biblioteker som `clj-time` eller `java-time`, som gir mer avanserte funksjoner for å håndtere datoer og tider.

## Se også:
- [Offisiell Clojure dokumentasjon for datoer og tider](https://clojure.github.io/java.time.javadoc/java/time/Instant.html)
- [Clj-time biblioteket](https://github.com/clj-time/clj-time)
- [Java-time biblioteket](https://github.com/dm3/clojure.java-time)