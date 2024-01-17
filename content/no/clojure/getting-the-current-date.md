---
title:                "Få gjeldende dato"
html_title:           "Clojure: Få gjeldende dato"
simple_title:         "Få gjeldende dato"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Få den nåværende datoen er en vanlig oppgave for programmører, da det er nødvendig for å spore og behandle tid i et program. Dette gjøres vanligvis ved å hente systemets nåværende dato og tid, og omformere den til et format som er lett å lese og bruke.

## Hvordan:
```Clojure
(def now (java.util.Date.))
(def sdf (java.text.SimpleDateFormat. "dd/MM/yyyy"))
(.format sdf now)
```

Output:
```Clojure
"12/12/2021"
```

## Dypdykk:
Før i tiden var det vanlig å bruke Java-koden ```System.currentTimeMillis()``` for å få den nåværende datoen. Men med introduksjonen av Java 8 i 2014, kom en ny og forbedret måte å håndtere tid på: Java 8 "time library". Dette biblioteket gir enklere og mer nøyaktig håndtering av datoer og klokkeslett.

Det finnes også alternative biblioteker som kan brukes i Clojure for å få den nåværende datoen, som for eksempel "clj-time" og "joda-time". Disse bibliotekene gir mer fleksibilitet og funksjonalitet, men innebærer også ekstra avhengigheter for prosjektet ditt.

Implementasjonsdetaljer:
I den viste koden bruker vi Java Date og SimpleDateFormat-klassene for å få og formatere den nåværende datoen. I Java 8 og nyere kan du også bruke de nye klassene fra "time library", som LocalDate og LocalDateTime, for å håndtere datoer og klokkeslett.

## Se også:
- Clojure dokumentasjon for "java.util.Date": https://clojuredocs.org/clojure.java-time/local-date
- Java 8 "time library" dokumentasjon: https://docs.oracle.com/javase/tutorial/datetime/iso/datetime.html