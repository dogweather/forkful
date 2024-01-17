---
title:                "Interpolering av en sträng"
html_title:           "Java: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
String interpolation är en vanlig teknik som används av programmerare för att kombinera variabler och text på ett smidigt sätt. Istället för att behöva skriva ut variablerna och sedan manuellt lägga till text runtom, kan man istället interpolera variablerna direkt in i en sträng. Det gör koden lättare att läsa och minskar risken för felaktig formatering.

## Så här:
```java
// Skapa variabler att interpolera
String name = "Anna";
int age = 25;

// Interpolera variablerna in i en sträng
String message = String.format("Hej, mitt namn är %s och jag är %d år gammal.", name, age);

// Skriv ut den interpolerade strängen
System.out.println(message);

// Output:
// Hej, mitt namn är Anna och jag är 25 år gammal.
```

## Djupdykning:
String interpolation är en slags syntaktisk sockersyrlösning (syntactic sugar) som först introducerades i programmeringsspråket Perl. Det finns flera alternativ för att interpolera strängar i Java, såsom `String.format()` som används i exemplet ovan, eller String concatenation (+ operatorn).

## Se även:
- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Perl Interpolation article](https://perldoc.perl.org/perldata.html#Scalar-value-interpolation)