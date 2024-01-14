---
title:                "Java: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Å undersøke om en mappe eksisterer kan være en viktig del av en javaprogrammerers arbeid. Dette kan være nyttig for å sikre at programmet fungerer riktig og for å unngå feil som kan oppstå når en mappe ikke er tilgjengelig.

## Hvordan
Det er flere måter å sjekke om en mappe eksisterer i Java. En enkel måte å gjøre dette på er ved hjelp av File-klassen. Følgende kode viser hvordan du kan sjekke om en mappe med navnet "testmappe" eksisterer på C-stasjonen:

```Java
File mappe = new File("C:\\testmappe");
boolean finnes = mappe.exists();
System.out.println("Finnes mappen: " + finnes);
```

Dersom mappen eksisterer vil output være: "Finnes mappen: true". Hvis mappen ikke eksisterer vil output være: "Finnes mappen: false".

En annen måte å sjekke om en mappe eksisterer er ved hjelp av metoden `exists()` fra klassen `Files` i Java NIO. Følgende kode viser hvordan du kan gjøre dette:

```Java
Path mappeSti = Paths.get("C:\\testmappe");
boolean finnes = Files.exists(mappeSti);
System.out.println("Finnes mappen: " + finnes);
```

Her også vil output variere avhengig av om mappen eksisterer eller ikke.

## Dypdykk
Det er viktig å merke seg at selv om en mappe eksisterer når du sjekker det på en gitt tid, så kan det hende at mappen slettes eller flyttes senere. Derfor bør man alltid være forsiktig når man stoler på at en mappe eksisterer før man bruker den i koden sin.

Noen ganger kan det også være lurt å sjekke om en mappe er tilgjengelig for skriving, ikke bare om den eksisterer. Dette kan gjøres ved å bruke metoden `isWritable()` fra File eller `isWritable()` fra Files.

## Se også
- [Java Documentation - File Class](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/io/File.html)
- [Java Documentation - Files Class](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/nio/file/Files.html)