---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Java: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen ønske å skrive ut debug-utdata? Vel, for å forenkle feilsøkingen! Ved å skrive ut relevante verdier og meldinger kan man raskt finne ut hvor det oppstår problemer i koden og deretter løse dem.

## Hvordan gjøre det
Her er et eksempel på hvordan man kan skrive ut en enkel melding i Java:

```Java
System.out.println("Dette er en debug-melding!");
```

Dette vil skrive ut teksten "Dette er en debug-melding!" i konsollen når man kjører programmet. Det er viktig å huske å inkludere `System.out` for å indikere at meldingen skal skrives ut i konsollen.

Man kan også skrive ut verdier av variabler ved å bruke `System.out.println()` kombinert med variabelnavnet. For eksempel:

```Java
int num = 5;
System.out.println("Verdien av num er: " + num);
```

Dette vil skrive ut "Verdien av num er: 5" i konsollen. Merk at `System.out.println()` tar inn en streng som argument, derfor må man bruke `+` for å kombinere strenger og variabler.

## Dykk dypere
Å skrive ut debug-utdata er spesielt nyttig når man jobber med større og mer komplekse programmer. Ved å skrive ut verdier i ulike deler av koden, kan man få en bedre forståelse av hvordan data flyter gjennom programmet og identifisere hvor problemer oppstår.

En nyttig teknikk er å bruke `System.out.println()` i kombinasjon med betingede uttrykk, for eksempel:

```Java
if (num < 10) {
    System.out.println("Num er mindre enn 10!");
} else {
    System.out.println("Num er større enn 10!");
}
```

Dette vil skrive ut en melding basert på verdien av `num`. Dersom verdien er mindre enn 10, skrives den første meldingen ut, ellers vil den andre meldingen bli skrevet ut.

Et annet tips er å bruke **logging**, som er en mer avansert måte å skrive ut informasjon i koden. Ved å bruke logging kan man også velge hvilke meldinger som skal skrives ut basert på ulike nivåer av kritikalitet.

## Se også
- [Java dokumentasjon om debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/java.html#setting_debug_options)
- [Java logging tutorial](https://www.baeldung.com/java-logging-intro)
- [Best practices for debugging Java code](https://www.udemy.com/blog/java-debugging-tips/)