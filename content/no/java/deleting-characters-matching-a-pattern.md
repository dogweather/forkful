---
title:    "Java: Slette tegn som matcher et mønster."
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte nødvendig å slette bestemte tegn som matcher et mønster når man jobber med tekstbehandling eller dataanalyse. Dette kan være for å rydde opp eller få ønsket format på teksten. I denne bloggposten vil vi se på hvordan vi kan gjøre dette ved hjelp av Java-programmering.

## Hvordan

For å slette tegn som matcher et bestemt mønster i en tekststreng, kan vi bruke metoden `replaceAll()` i Java. Denne metoden tar to parametere - det første er mønsteret som skal matches og det andre er teksten som skal endres. La oss se på et eksempel:

```Java
String tekst = "Jeg spiser epler hver dag.";
String endretTekst = tekst.replaceAll("epler", "bananer");
System.out.println(endretTekst);
```

Dette vil gi oss følgende utskrift:

```
Jeg spiser bananer hver dag.
```

Som du kan se, har metoden `replaceAll()` erstattet alle forekomster av "epler" med "bananer" i teksten vår. Hvis du ønsker å slette et bestemt tegn, for eksempel alle tall i teksten, kan du også bruke et regulært uttrykk som matcher tall. La oss se på et annet eksempel:

```Java
String tallTekst = "Jeg er 25 år gammel.";
String endretTekst = tallTekst.replaceAll("\\d", "");
System.out.println(endretTekst);
```

Dette vil gi oss følgende utskrift:

```
Jeg er år gammel.
```

Her har vi brukt metakarakteren `\d` som betyr "alle tall", og ved å erstatte den med et tomt tegn "", har vi slettet alle tall fra teksten.

## Dypdykk

Det finnes mange forskjellige metakarakterer som kan brukes når vi matcher tegn i en tekststreng. Noen eksempler er `\d` for tall, `\w` for bokstaver og `\s` for mellomrom. Disse kan også kombineres, for eksempel `\w\d` som vil matche et tall etterfulgt av en bokstav. Det er også mulig å bruke negasjon, for eksempel `[^0-9]` vil matche alt som ikke er tall. Ved å utforske og leke med disse metakarakterene, kan du gjøre avansert og presis tekstbehandling i Java.

## Se også

- [Oracle Java documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Regular-Expressions.info](https://www.regular-expressions.info/) - en omfattende ressurs for å lære mer om regulære uttrykk.
- [Regex101](https://regex101.com/) - en praktisk online verktøy for å teste og forstå regex-uttrykk.