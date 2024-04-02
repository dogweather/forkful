---
date: 2024-01-20 17:45:52.568662-07:00
description: "Att extrahera substr\xE4ngar inneb\xE4r att man plockar ut en specifik\
  \ del av en textstr\xE4ng. Programmerare g\xF6r detta f\xF6r att bearbeta, validera\
  \ eller manipulera\u2026"
lastmod: '2024-03-13T22:44:37.776692-06:00'
model: gpt-4-1106-preview
summary: "Att extrahera substr\xE4ngar inneb\xE4r att man plockar ut en specifik del\
  \ av en textstr\xE4ng. Programmerare g\xF6r detta f\xF6r att bearbeta, validera\
  \ eller manipulera\u2026"
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## Vad & Varför?
Att extrahera substrängar innebär att man plockar ut en specifik del av en textsträng. Programmerare gör detta för att bearbeta, validera eller manipulera data mer effektivt.

## Hur man gör:
Extrahera en delsträng med `substring()`-metoden:

```java
public class SubstringExample {
    public static void main(String[] args) {
        String text = "Hej, världen!";
        String deltext = text.substring(4, 11); // börjar på index 4, slutar före index 11
        System.out.println(deltext); // Skriver ut "världen"
    }
}
```

Resultat:
```
världen
```

## Fördjupning:
Metoden `substring()` har använts sedan de tidiga versionerna av Java. Ett alternativ är att använda `StringUtils` från Apache Commons Lang för fler möjligheter och kontroller. Detaljerna i hur substring extraheras har förbättrats över tid för effektivitet, initialt skapade det nya strängar med nya teckenarrayer, nu skapar det en ny sträng som delar samma teckenarray som ursprungssträngen men med olika start och slutindex.

## Se Också:
- [String documentation i Java](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
