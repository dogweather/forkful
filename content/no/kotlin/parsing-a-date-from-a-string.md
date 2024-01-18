---
title:                "Analyse av dato fra en streng"
html_title:           "Kotlin: Analyse av dato fra en streng"
simple_title:         "Analyse av dato fra en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Oversettelse av en dato fra en streng er en måte å konvertere en streng, som inneholder informasjon om en dato, til et format som datamaskinen kan forstå. Dette gjør det enklere for programmerere å arbeide med datoer og utføre forskjellige operasjoner på dem.

# Hvordan:

Det er flere måter å parse en dato fra en streng i Kotlin, avhengig av formatet til strengen. Her er noen eksempler ved hjelp av standardbiblioteket til Kotlin:

```Kotlin
// Parse dato fra streng i formatet "dd/MM/yyyy"
val dato = LocalDate.parse("12/03/2020", DateTimeFormatter.ofPattern("dd/MM/yyyy"))
```
Resultat: 2020-03-12

```Kotlin
// Parse dato fra streng i formatet "yyyy-MM-dd"
val dato = LocalDate.parse("2020-03-14")
```
Resultat: 2020-03-14

```Kotlin
// Parse dato og tidspunkt fra streng i formatet "yyyy-MM-dd HH:mm:ss"
val datoTid = LocalDateTime.parse("2020-03-15 13:15:00")
```
Resultat: 2020-03-15T13:15:00

```Kotlin
// Parse dato og tidspunkt fra streng i formatet "dd/MM/yyyy HH:mm"
val datoTid = LocalDateTime.parse("15/03/2020 13:30", DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm"))
```
Resultat: 2020-03-15T13:30:00

# Dypdykk:

Parsing av datoer fra strenger er et vanlig problem i programmering, spesielt når man arbeider med brukerinput eller data fra eksterne kilder. Historisk sett var parsing av datoer en komplisert oppgave, men med moderne programmeringsspråk og biblioteker, som Kotlin, har dette blitt mye enklere.

Det finnes flere alternativer til å parse datoer i Kotlin, som for eksempel å bruke regex eller bygge en egen parser. Disse alternativene krever imidlertid mer arbeid og kan være vanskeligere å vedlikeholde.

Implementeringen av datoparsing i Kotlin er basert på Java sin [java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) pakke. Dette gjør det mulig å bruke de samme funksjonene og formatene som i Java.

# Se også:

- [Dokumentasjon for datoparsing i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-decimal-format/)