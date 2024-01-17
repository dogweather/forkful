---
title:                "Hente gjeldende dato"
html_title:           "Java: Hente gjeldende dato"
simple_title:         "Hente gjeldende dato"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Når vi snakker om "dagens dato", refererer vi til den nåværende datoen på kalenderen. Dette er en vanlig oppgave for programmerere, da det er nødvendig å håndtere datoer og tidspunkter i mange applikasjoner.

## Slik gjør du det:

Java tilbyr en innebygget klasse kalt "LocalDate" som lar deg få tak i dagens dato. Du kan bruke metoden "now()" for å få tak i dagens dato-objekt, og deretter bruke metoder som "getDayOfMonth" eller "getMonth" for å få tak i spesifikke data. Her er et eksempel:

```Java
LocalDate dagensDato = LocalDate.now();
System.out.println("Dagens dato er " + dagensDato.getDayOfMonth() + "/" + dagensDato.getMonthValue() + "/" + dagensDato.getYear());
```

Dette vil gi følgende utdata:

```Text
Dagens dato er 06/10/2021
```

## Dykke dypere:

Før Java 8, var det ikke så enkelt å få tak i dagens dato. Utviklere måtte bruke Date og Calendar-klassene, noe som kunne være ganske kronglete og ressurskrevende. Med introduksjonen av LocalDate-klassen i Java 8, ble dette mye enklere.

Det finnes også alternative biblioteker som Joda Time for å håndtere datoer og tidspunkter i Java. Disse bibliotekene gir mer funksjonalitet og fleksibilitet, men det kan være overflødig for enkle oppgaver som å få tak i dagens dato.

## Se også:

- [Oracle's LocalDate-dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Joda Time dokumentasjon](https://www.joda.org/joda-time/)