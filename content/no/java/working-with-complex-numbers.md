---
title:                "Å jobbe med komplekse tall"
date:                  2024-01-26T04:42:20.971162-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Komplekse tall utvider den reelle tallinjen gjennom tillegg av en imaginær enhet, `i`, der `i^2 = -1`. De er avgjørende i felter som ingeniørvitenskap, fysikk og avansert matematikk, hvor de modellerer fenomener som reelle tall ikke kan håndtere, som elektriske strømmer og signalbehandling.

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
