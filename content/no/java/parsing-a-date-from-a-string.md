---
title:                "Å analysere en dato fra en streng"
html_title:           "Java: Å analysere en dato fra en streng"
simple_title:         "Å analysere en dato fra en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å parse en dato fra en streng betyr å konvertere en tekstrepresentasjon av en dato til et Java Date objekt. Dette er nødvendig for å kunne behandle og manipulere datoer i programmering, for eksempel for å sortere eller filtrere dem.

Hvordan:

```Java
String stringDato = "12-04-2021";
Date dato = new SimpleDateFormat("dd-MM-yyyy").parse(stringDato);
System.out.println(dato);
```

Dette eksempelet viser hvordan man kan bruke SimpleDateFormat-klassen til å parse en streng til et Date objekt. Resultatet vil være "Mon Apr 12 00:00:00 CEST 2021".

Dypdykk:

Å parse datoer fra tekst er en vanlig utfordring i programmering, spesielt når man arbeider med brukerinput eller data lagret i tekstfiler. Før Java 8, måtte man bruke klasser som SimpleDateFormat eller DateFormat for å gjøre dette, men med Java 8 ble det introdusert en ny java.time pakke som gjorde det enklere å arbeide med datoer og tider.

Alternativt, hvis man ikke trenger å arbeide med tidssoner eller trenger mer fleksibilitet, kan man bruke LocalDate-klassen som gjør det mulig å parse datoer uten å bry seg om klokkeslett.

Viktig å vite når du parser datoer fra strenger er at det kan oppstå feil hvis formatet ikke matcher strengen. Derfor er det viktig å forstå både formatet for strengen og hvordan det tolkes av parser-klassen du velger å bruke.

Se også:

- Java SimpleDateFormat API: https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Java LocalDate API: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html