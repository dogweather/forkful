---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søk og erstatning av tekst er en metode for å identifisere og endre spesifikke strømmer av tekst. Dette er en viktig oppgave i programmering fordi den lar oss manipulere data på en effektiv og fleksibel måte.

## Hvordan gjøre det:
Her er et eksempel på hvordan man kan søke og erstatte tekst i Java.

```Java
public class Main {
 public static void main(String[] args) {
  String tekst = "Hei, verden!";
  String søkTekst = "verden";
  String erstatningTekst = "Norge";
  
  String nyTekst = tekst.replace(søkTekst, erstatningTekst);
  System.out.println(nyTekst);
 }
}
```
Når du kjører koden ovenfor, vil output bli:

```
Hei, Norge!
```
## Dypdykk
Historisk sett har søk og erstatning av tekst vært en kritisk funksjon i mange programmeringsspråk, inkludert Java. I eldre versjoner av Java måtte vi lage egne funksjoner for å gjøre dette, men i dag kan vi bare bruke innebygde metoder som `replace()`.

Det finnes alternative måter å søke og erstatte tekst på, for eksempel ved bruk av regelmessige uttrykk eller `StringBuffer` og `StringBuilder` klasser, men `replace()` gir den mest direkte tilnærmingen.

I praksis skanner `replace()`-metoden teksten sekvensielt fra start til slutt for å finne matchende sekvenser, og deretter erstatte dem. Det er viktig å merke seg at denne metoden er "hensynsløs," slik at den erstatter alle matchende sekvenser, ikke bare den første den finner.

## Se også
For mer informasjon om dette emnet, se følgende ressurser:
- JavaDocs for `String`-klassen: https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html
- Tutorial for å bruke regulære uttrykk i Java: https://www.vogella.com/tutorials/JavaRegularExpressions/article.html
- Guide til `StringBuffer` og `StringBuilder` klassene i Java: https://www.baeldung.com/java-string-builder-string-buffer