---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:52.828225-07:00
description: "Reguliere expressies (regex) zijn patronen die worden gebruikt om karaktercombinaties\
  \ in tekst te vinden. Programmeurs gebruiken ze voor het zoeken,\u2026"
lastmod: '2024-02-25T18:49:48.013834-07:00'
model: gpt-4-0125-preview
summary: "Reguliere expressies (regex) zijn patronen die worden gebruikt om karaktercombinaties\
  \ in tekst te vinden. Programmeurs gebruiken ze voor het zoeken,\u2026"
title: Reguliere expressies gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies (regex) zijn patronen die worden gebruikt om karaktercombinaties in tekst te vinden. Programmeurs gebruiken ze voor het zoeken, bewerken of manipuleren van strings op een efficiënte manier—tijd en regels code besparend.

## Hoe:
Om regex in Java te gebruiken, heb je de klassen `Pattern` en `Matcher` nodig uit `java.util.regex`. Hier is een voorbeeld van het vinden van e-mailadressen in een string.

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String tekst = "Contacteer me op hello@world.com of buzz@space.net.";
        String emailRegex = "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b";

        Pattern patroon = Pattern.compile(emailRegex);
        Matcher matcher = patroon.matcher(tekst);

        while (matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
Uitvoer:
```
hello@world.com
buzz@space.net
```

## Diepe Duik
Reguliere expressies bestaan sinds de jaren 1950, uitgevonden door wiskundige Stephen Kleene. Java heeft regex geïntegreerd sinds versie 1.4. Hoewel krachtig, kan regex overkill zijn voor eenvoudige stringoperaties—methoden zoals `String.contains()`, `String.split()`, en `String.startsWith()` zijn eenvoudige alternatieven voor basisscenario's. Onder de motorkap compileert Java's regex-engine (met behulp van `Pattern` en `Matcher`) het patroon in een reeks bytecode-instructies die door de `Matcher` tegen de invoerstring worden uitgevoerd.

## Zie Ook
Verken meer over regex in Java met deze bronnen:
- [Java Pattern Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Java Matcher Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Oracle Java Tutorial: Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular-Expressions.info voor een diepe duik in regex-syntax en patronen](https://www.regular-expressions.info/)
