---
title:    "Clojure: Konvertering av dato til streng"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med dataverdenen, er det ofte nødvendig å konvertere data i ulike formater. Dette kan være for å tilpasse data til ulike systemer eller for å gjøre det mer leselig for mennesker. En vanlig konvertering er å omgjøre en dato til en streng, og i denne artikkelen vil vi se på hvordan man gjør dette i Clojure.

## Hvordan gjøre det

Det er flere måter å konvertere en dato til en streng i Clojure, avhengig av hva slags format man ønsker. La oss starte med en enkel kode som konverterer dagens dato til en streng:

```Clojure
(let [current-date (java.util.Date.)]
  (str current-date))
```

Output:

```
"Wed Jul 08 20:47:33 CEST 2020"
```

I dette tilfellet bruker vi funksjonen `str` for å konvertere datoen til en standard streng som viser dag, måned, tid og tidssone.

Vi kan også bruke funksjonen `format` for å spesifisere ønsket format. For eksempel, hvis vi ønsker dato og tid på formatet "åååå/mm/dd - tt", kan vi bruke følgende kode:

```Clojure
(let [current-date (java.util.Date.)]
  (format "yyyy/MM/dd - hh:mm a" current-date))
```

Output:

```
"2020/07/08 - 20:47 PM"
```

Det finnes mange andre tilgjengelige formater som kan spesifiseres ved hjelp av funksjonen `format`. Du kan finne en liste over disse i dokumentasjonen for `java.util.SimpleDateFormatter`.

## Dykk dypere

Når vi konverterer en dato til en streng, er det viktig å forstå hva slags type vi ønsker å få ut som resultat. I eksemplene ovenfor brukte vi funksjonen `str`, som konverterer datoen til en enkel streng. Hvis man ønsker å lagre datoen i en spesiell form, som for eksempel å hente ut et spesifikt element som dag, måned eller år, må man først konvertere datoen til en annen type. Dette kan gjøres ved hjelp av funksjonen `local-date-time`, som gjør om datoen til et objekt av typen `java.time.LocalDateTime`. Dette gir oss muligheten til å hente ut individuelle elementer ved hjelp av funksjoner som `get-year` og `get-month`.

## Se også

- [Clojure dokumentasjon](https://clojuredocs.org/)
- [Java dokumentasjon for `java.util.Date`](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java dokumentasjon for `java.util.SimpleDateFormatter`](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)