---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Sammenkjedning av strenger i Java betyr å koble to eller flere strenger sammen. Dette er utrolig nyttig når du vil lage komplekse strenger fra mindre deler.

---

## Hvordan:

Her er et enkelt eksempel på hvordan du kan kjede sammen strenger i Java:

```Java
public class Main {
    public static void main(String[] args) {
        String str1 = "Hallo";
        String str2 = "Norge";
        String str3 = str1 + " " + str2;
        System.out.println(str3);
    }
}
```

Når du kjører koden over, vil utskriften være:

```
Hallo Norge
```

---

## Dypdykk:

### Historisk kontekst
Operatøren `+` i Java har vært i stand til å sammenkjede strenger siden den første versjonen av språket ble lansert. Men med utgivelsen av Java 5 introduserte de `StringBuilder`-klassen som leverer forbedret ytelse for større strengoperasjoner.

### Alternativer
I tillegg til operatøren `+` og `StringBuilder`, kan du også bruke `StringBuffer` og `String.format()`. Disse alternativene har unike bruksområder, avhengig av dine behov.

### Implementeringsdetaljer
Når du bruker operatøren `+` for å sammenkjede strenger, lager Java i realiteten nye String-objekter, siden String i Java er uforanderlig. Dette kan føre til et ineffektivt minnebruk hvis du kjeder sammen mange strenger. `StringBuilder` og `StringBuffer` kan være mer effektive i disse scenarioene siden de tillater endringer på de opprinnelige objektene.

---

## Se Også:

- Java Dokumentasjon av StringBuilder - [https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html)
- Java Tutorial om Strenger - [https://docs.oracle.com/javase/tutorial/java/data/strings.html](https://docs.oracle.com/javase/tutorial/java/data/strings.html)