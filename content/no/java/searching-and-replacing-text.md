---
title:                "Søke og erstatte tekst"
html_title:           "Java: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Why: Hvorfor vil noen endre tekst?

Hvis du er en Java-utvikler, har du sannsynligvis støtt på situasjoner der du må søke og erstatte tekst i koden din. Dette kan være for å endre variabelnavn, rette opp skrivefeil eller gjøre større endringer. Uansett årsaken er det viktig å kunne beherske søke- og erstattingsfunksjonaliteten i Java for å effektivisere arbeidet ditt og sikre korrekt kode.

## How To: Slik søker og erstatter du tekst i Java

For å søke og erstatte tekst i Java, kan du bruke metoden `String.replaceAll()` som tar inn to parameterstrenge: en regex (regulært uttrykk) som angir teksten du vil søke etter, og en tekst som skal erstatte den. Her er et eksempel på en regex som søker etter alle forekomster av ordet "hallo" og erstatter det med "hei":

```Java
String utgangstekst = "Hallo verden! Hallo alle sammen!";
String erstattetTekst = utgangstekst.replaceAll("hallo", "hei");
System.out.println(erstattetTekst);
```

Dette vil gi følgende output:

```
Hei verden! Hei alle sammen!
```

Merk at `replaceAll()`-metoden tar inn regex som første parameter, så det kan være lurt å lese seg opp på hvordan regex fungerer for å få en bedre forståelse av hvordan du kan søke etter spesifikke tekststrenger.

## Deep Dive: Dypere informasjon om søking og erstatting av tekst

I Java kan du bruke både `replace()` og `replaceAll()`-metodene for å søke og erstatte tekst, men det er noen forskjeller mellom dem. Mens `replace()` bare erstatter den første forekomsten av din angitte tekst, vil `replaceAll()` bytte ut alle forekomster som matcher din regex.

Du kan også bruke `String.replaceFirst()` for å erstatte den første forekomsten av teksten, men det er viktig å bemerke at denne metoden også tar inn regex som parameter. Så hvis du vil erstatte en konkret tekststreng, må du enten bruke `replace()` eller sørge for å formatere teksten din som et regex-uttrykk.

Når du bruker `replaceAll()`, vil metoden returnere en ny string med alle forekomster erstattet, mens den originale stringen forblir den samme. Hvis du ønsker å erstatte tekst i den eksisterende stringen, kan du bare tilordne den til en ny variabel eller bruke `String.replace()`.

See Also: Se også disse nyttige ressursene:

- Dokumentasjon for `String.replaceAll()`: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String)
- Intro til regulære uttrykk: https://www.tutorialspoint.com/java/java_regular_expressions.htm
- Eksempler på Java-regulære uttrykk: https://www.javatpoint.com/java-regex