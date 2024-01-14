---
title:    "Java: Utskrift av feilsøkingsutdata"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Hvorfor

Det kan være fristende å hoppe over å inkludere debug utskrift i koden din, men det kan være en svært nyttig praksis når du utvikler et Java-program. Utskrift av feilmeldinger og variabler kan hjelpe deg med å feilsøke og forstå hva som skjer i koden din.

# Hvordan

Å legge til debug utskrift i Java-koden din er enkelt. Du kan bruke "System.out.println()" metoden for å skrive ut en variabel eller en melding til konsollen.

```Java
String navn = "Maria";
System.out.println("Hei, mitt navn er " + navn);
```
Dette vil skrive ut "Hei, mitt navn er Maria" i konsollen når programmet kjører.

Du kan også bruke "System.out.printf()" metoden for å skrive ut en formattert melding eller variabel.

```Java
int tall = 10;
System.out.printf("Tallet er %d", tall);
```
Dette vil skrive ut "Tallet er 10" i konsollen.

Debug utskrift kan også hjelpe deg med å følge med på hva som skjer i koden mens den kjører. Du kan plassere utskriftssetninger i forskjellige deler av koden for å se hvordan variabler endres eller hvilke deler av koden som blir utført.

# Dypdykk

Det er viktig å merke seg at debug utskrift bør fjernes før programmet ditt er ferdig og klart for produksjon. Utskriftssetningene kan forstyrre ytelsen til programmet ditt og kan føre til unødvendig langsomhet.

I tillegg bør du vurdere å bruke en logging-bibliotek som log4j i stedet for å bruke standard utskriftsfunksjonene. Logging-biblioteker gir deg mer kontroll over hva som skal skrives ut, kan lagre utskrifter i en fil og kan være nyttig for feilsøking i produksjonsmiljøer.

# Se også

* [Java Debugging with IntelliJ IDEA](https://www.jetbrains.com/help/idea/creating-and-running-your-first-java-application.html#debug)
* [Debugging in Eclipse: Java’s Debugging Tool](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article1.php)
* [Java Logging Basics](https://www.tutorialspoint.com/log4j/log4j_logging_basics.htm)