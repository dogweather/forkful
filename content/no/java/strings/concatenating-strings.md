---
date: 2024-01-20 17:34:59.188104-07:00
description: "I Java smelter vi sammen tekststrenger ved \xE5 konkatere dem. Dette\
  \ gj\xF8r vi for \xE5 bygge setninger, dynamiske meldinger eller for \xE5 sl\xE5\
  \ sammen data til en\u2026"
lastmod: '2024-03-13T22:44:40.658200-06:00'
model: gpt-4-1106-preview
summary: "I Java smelter vi sammen tekststrenger ved \xE5 konkatere dem."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## How to:
```java
public class StringConcatExample {
    public static void main(String[] args) {
        String hello = "Hei, ";
        String world = "verden!";
        String greeting = hello + world; // String concatenation
        System.out.println(greeting); // Prints: Hei, verden!
        
        // Med StringBuilder for lengre eller mer komplekse operasjoner
        StringBuilder sb = new StringBuilder();
        sb.append(hello).append(world);
        System.out.println(sb.toString()); // Prints: Hei, verden!
    }
}
```

## Deep Dive
Konkatenering av strenger har vært en del av Java helt siden starten. Tidligere var det vanlig å bruke `+` operatoren, men dette kan bli ineffektivt i løkker. Modernere tilnærminger som `StringBuilder` optimaliserer ytelsen ved å unngå unødvendig opprettelse av mange midlertidige strengobjekter.

Det finnes andre alternativer også, som `StringBuffer` for trådsikker konkatenering og `String.format` for å slå sammen med variabler og formatterte verdier. Siden Java 8, har man også `StringJoiner` for å lage kommaseparerte lister eller andre strukturer.

Under kjøring, konverterer Java strengkonkatenering til `StringBuilder` operasjoner i bakgrunnen vha. en prosess kalt "string interning", som hjelper med å spare minne.

## See Also
- Oracle's official documentation on strings: https://docs.oracle.com/javase/tutorial/java/data/strings.html
- StringBuilder JavaDoc: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html
- Effective Java, Third Edition by Joshua Bloch, for best practices with strings and more.
