---
title:                "Java: Sammenføyning av tekststrenger"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmering trenger vi å kombinere flere tekststrenger til en enkelt, lengre streng. Dette prosessen kalles "concatenation" på engelsk, eller "konkatenering" på norsk. Det kan være nyttig når vi for eksempel ønsker å skrive ut en tekst med variabler, eller når vi trenger å bygge URLer.

## Slik gjør du det

Å konkatenerer strenger i Java er enkelt, og kan gjøres på flere måter. En måte er å bruke operatøren "+" for å legge sammen to strenger. Se eksempelet under:

```Java
String navn = "Ingrid";
String etternavn = "Nilsen";

String navnSammen = navn + etternavn; // Resultatet blir "IngridNilsen"
```

En annen måte er å bruke String-metoden `concat()`. Her passer det også å inkludere en variabel og en konstant tekststreng:

```Java
String navn = "Ole";
String alder = "30 år";

String tekst = navn.concat(" er ").concat(alder); // Resultatet blir "Ole er 30 år"
```

Merk at det også går an å bruke `concat()`-metoden sammen med numeriske variabler ved å konvertere dem til strenger først.

## Dypere dykk

I Java blir hver streng representert som et objekt av typen `String`. Når vi bruker operatøren "+" eller `concat()`-metoden, blir det egentlig laget et nytt objekt med den sammenslåtte strengen. Dette skjer fordi strenger i Java er uforanderlige, så det gamle objektet kan ikke bare endres.

Derfor kan det være mer effektivt å bruke `StringBuilder`-klassen til å utføre konkatenering. Denne klassen lar deg endre strenger i stedet for å lage nye objekter hele tiden. Her er et eksempel på hvordan det kan gjøres:

```Java
StringBuilder sb = new StringBuilder();

sb.append("Hei, ").append("er du ").append("klar for helgen?"); // Resultatet blir "Hei, er du klar for helgen?"

String resultat = sb.toString(); // Konverterer StringBuilder-objektet til en vanlig streng og lagrer det i en variabel
```

Bruken av `StringBuilder` er spesielt nyttig når vi trenger å konkatenerer mange strenger, for eksempel i en løkke.

## Se også

- [Java String Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Java String Concatenation Tutorial](https://www.baeldung.com/java-string-concatenation)