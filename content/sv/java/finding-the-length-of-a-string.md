---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng innebär att räkna antalet tecken den innehåller. Programmerare gör detta för att manipulera strängar effektivare när de kodar.

## Så här gör du:

För att hitta en stränglängd i Java använder du `length()` metoden. Ta en titt på följande exempel:

```Java
public class Main {
    public static void main(String[] args) {
        String hej = "Hej Sverige!";
        int length = hej.length();
        System.out.println("Stränglängden är: " + length);
    }
}
```
 När du kör koden ovan, kommer output att vara:

```
Stränglängden är: 12
```

## Djupdykning

Hämtning av en strängs längd har varit en grundläggande operation sedan programmeringens tidiga dagar. I tidiga språk, inklusive C och Assembly, var det dock nödvändigt att iterera genom en sträng karaktär för karaktär tills ett nullvärde (eller "end of string"-tecken) hittades. Javas inbyggda `length()`-metod gör att du kan hoppa över denna potentiellt tidskrävande process.

Alternativt kan du använda `codePointCount` metoden om din sträng inkluderar Unicode-tecken. Observera dock att `codePointCount` kan ge olika resultat jämfört med `length()`.

Implementationen av `length()` i klassen String i Java är ganska enkel. Metoden returnerar helt enkelt värdet av en intern variabel som håller reda på strängens längd – ingen loop behövs.

## Se även:

För mer information om metoden `length()` och dess användning, besök Javas officiella dokumentation: [https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html#length()](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html#length())

För en grundlig jämförelse mellan `length()` och `codePointCount()`, se denna StackOverflow-tråd: [https://stackoverflow.com/questions/12437545/difference-between-string-length-and-string-codepointcount-in-java](https://stackoverflow.com/questions/12437545/difference-between-string-length-and-string-codepointcount-in-java)