---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# ## Hva & Hvorfor?
Å trekke ut understrenger handler om å hente spesifikke deler fra en større streng. Programmerere gjør dette for å isolere og manipulere data på mer effektive måter.

# ## Hvordan:
Her er et enkelt eksempel på hvordan du kan hente ut en understreng i Java.

```Java
public class Main {
    public static void main(String[] args) {
        String ord = "Hallo, verden!";
        String understreng = ord.substring(7, 13);
        System.out.println(understreng);
    }
}
```
I eksempelet over vil output være "verden".

# ## Dypdykk
Historisk sett har substrings vært en viktig del av strengmanipuleringen siden tidlige programmeringsspråk som ALGOL og C. 

Det er ulike måter å hente ut understrenger på i Java, `substring()` er bare én av dem. En annen metode kan være å bruke `split()` metoden, som deler en streng i flere deler basert på en gitt skilletegn.

Imidlertid er `substring()` den mest direkte metoden og den beste løsningen for enkle behov. Dette er fordi `substring()` i de fleste tilfeller har beste ytelse siden den bare trenger å lage en ny referanse til den opprinnelige strengen, og ikke lage nye strenger som i `split()`.

# ## Se også:
For mer detaljert informasjon, kan du besøke disse nettstedene:

- [Oracle sin Java dokumentasjon for String metoder](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [Java String substring() Metode med Eksempler](https://www.javatpoint.com/java-string-substring)
- [Java String split() metode med eksempler](https://www.javatpoint.com/java-string-split)