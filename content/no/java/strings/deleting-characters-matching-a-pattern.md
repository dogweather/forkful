---
date: 2024-01-20 17:42:32.731276-07:00
description: "Hvordan: Her bruker vi `replaceAll`-metoden til \xE5 fjerne alle tall\
  \ fra en tekststreng, hvor `\\\\d+` er et regul\xE6rt uttrykk som matcher en eller\
  \ flere\u2026"
lastmod: '2024-04-05T21:53:41.631131-06:00'
model: gpt-4-1106-preview
summary: "Her bruker vi `replaceAll`-metoden til \xE5 fjerne alle tall fra en tekststreng,\
  \ hvor `\\\\d+` er et regul\xE6rt uttrykk som matcher en eller flere siffer."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Hvordan:
```java
import java.util.regex.Pattern;

public class PatternRemoval {
    public static void main(String[] args) {
        String originalText = "Dette123 er456 et789 eksempel.";
        String modifiedText = originalText.replaceAll("\\d+", "");
        System.out.println(modifiedText); // Skriver ut: "Dette er et eksempel."
    }
}
```
Her bruker vi `replaceAll`-metoden til å fjerne alle tall fra en tekststreng, hvor `\\d+` er et regulært uttrykk som matcher en eller flere siffer.

## Deep Dive
Sletting av tegn etter mønster har røtter i tidlig bruk av regulære uttrykk i Unix-verdenen. Alternativer inkluderer bruk av `String.replace` for enkel erstatning uten mønstergjenkjenning, eller `Pattern` og `Matcher` klassene for mer kompleks tekstbehandling. Implementasjonsdetaljer krever forståelse av regulære uttrykk: sekvenser som definerer et søkemønster. I Java er disse kraftige verktøyene innkapslet i `java.util.regex`-biblioteket.

## Se Også
- [Java String `replaceAll`](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String))
- [Regulære uttrykk i Java](https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/util/regex/Pattern.html)
- [Oracle Tutorial - Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
