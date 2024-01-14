---
title:                "Java: Konvertering av en dato til en streng"
simple_title:         "Konvertering av en dato til en streng"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange ganger i et program, vil du trenge å konvertere en dato til en streng. Dette er spesielt nyttig når du vil vise en dato i et mer leselig format for brukeren. Å kunne konvertere en dato til en streng er en nyttig ferdighet å ha i Java-programmering.

## Hvordan gjøre det
Å konvertere en dato til en streng i Java er ganske enkelt. Du kan bruke SimpleDateFormat-klassen for å formatere datoen i ønsket format. Her er et eksempel på hvordan du kan konvertere en dato til en streng:

```Java
Date dato = new Date();
SimpleDateFormat formatter = new SimpleDateFormat("dd.MM.yyyy");
String datoSomStreng = formatter.format(dato);
System.out.println(datoSomStreng);
```
**Output: 25.03.2020**

I dette eksempelet har vi brukt SimpleDateFormat til å formatere datoen som en streng i "dd.MM.yyyy" format. Du kan endre formateringen ved å endre formatet i parentesen. Her er noen eksempler på ulike formater og deres tilhørende output.

```Java
SimpleDateFormat formatter = new SimpleDateFormat("MM/dd/yyyy");
```
**Output: 03/25/2020**

```Java
SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
```
**Output: 2020-03-25**

Som du kan se, kan du formatere datoen på forskjellige måter ved å endre formatet i SimpleDateFormat. Dette gjør det enkelt å tilpasse datoen til dine spesifikke behov.

## Dypdykk
Når du bruker SimpleDateFormat, er det viktig å være oppmerksom på noen av formatalternativene som er tilgjengelige. For eksempel bruker "M" for måned stor bokstav, mens "m" for bruker små bokstaver. Dette er fordi "M" brukes for å representere en numerisk måned, mens "m" brukes for en tekstlig måned. Hvis du bruker feil bokstav, vil du få feil output.

I tillegg er det verdt å merke seg at du kan legge til andre bokstaver eller tegn i formatteringsstrengen for å få ønsket output. For eksempel, hvis du vil inkludere dag i uken i output, kan du legge til "EEE" i formatteringsstrengen. Her er noen eksempler:

```Java
SimpleDateFormat formatter = new SimpleDateFormat("EEE, MM/dd/yyyy");
```
**Output: Wed, 03/25/2020** (hvis datoen er en onsdag)

```Java
SimpleDateFormat formatter = new SimpleDateFormat("EEE, dd.MM.yyyy");
```
**Output: Wed, 25.03.2020** (hvis datoen er en onsdag)

Videre kan du også angi et bestemt språk for dager og måneder ved å bruke Locale-klassen. For eksempel, hvis du vil at datoene skal vises på norsk, kan du bruke følgende kode:

```Java
SimpleDateFormat formatter = new SimpleDateFormat("EEE, dd. MMMM yyyy", new Locale("nb", "NO"));
```
**Output: Wed, 25. mars 2020** (hvis datoen er en onsdag)

## Se Også
- [Java SimpleDateFormat Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Locale-klassen Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)