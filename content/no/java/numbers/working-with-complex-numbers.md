---
date: 2024-01-26 04:42:20.971162-07:00
description: "Hvordan: Java har ikke innebygd st\xF8tte for komplekse tall, men vi\
  \ kan lage v\xE5r egen klasse eller bruke et bibliotek. Her er et raskt eksempel\
  \ p\xE5 hvordan\u2026"
lastmod: '2024-03-13T22:44:40.660456-06:00'
model: gpt-4-0125-preview
summary: "Java har ikke innebygd st\xF8tte for komplekse tall, men vi kan lage v\xE5\
  r egen klasse eller bruke et bibliotek."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Java har ikke innebygd støtte for komplekse tall, men vi kan lage vår egen klasse eller bruke et bibliotek. Her er et raskt eksempel på hvordan du oppretter en enkel `ComplexNumber` klasse og bruker den:

```java
public class ComplexNumber {
    private double real;
    private double imaginary;

    public ComplexNumber(double real, double imaginary) {
        this.real = real;
        this.imaginary = imaginary;
    }

    public ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
    }

    // ToString for å vise komplekse tall i a + bi form
    @Override
    public String toString() {
        return String.format("%.1f + %.1fi", real, imaginary);
    }

    // Rask test
    public static void main(String[] args) {
        ComplexNumber c1 = new ComplexNumber(2, 3);
        ComplexNumber c2 = new ComplexNumber(1, 4);

        System.out.println("Sum: " + c1.add(c2));
    }
}
```

Eksempelutdata for hovedmetoden vil være:

```
Sum: 3.0 + 7.0i
```

## Dypdykk
Før høynivåspråk som Java, jobbet programmerere direkte med matematikkbiblioteker i språk som Fortran eller C for å håndtere komplekse operasjoner. Konseptet sporer tilbake til det 16. århundret, kreditert matematikere som Gerolamo Cardano og Rafael Bombelli.

I Java er `java.lang.Math` et sted å gå for det essensielle, men hopper over komplekse tall, sannsynligvis fordi ikke hver programmerer bruker dem. Alternativer? Bruk biblioteker. Apache Commons Math tilbyr en `Complex` klasse pakket med metoder for manipulasjon. Her er hvorfor det er stilig å rulle din egen: Lettvektig, skreddersydd til dine eksakte behov, og ingen biblioteksoverhod.

En viktig detalj: pass på presisjonen til flyttall. Datamaskiner kan ikke representere noen tall nøyaktig, noe som fører til avrundingsfeil. Når du utfører repeterende komplekse operasjoner, kan disse feilene akkumuleres!

## Se Også
For dypere dykk og mer komplekse operasjoner, sjekk:

- [Apache Commons Math](https://commons.apache.org/proper/commons-math/)
- [JScience's Complex class](http://jscience.org/)
- Oracles opplæringer om [flyttallsaritmetikk](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
