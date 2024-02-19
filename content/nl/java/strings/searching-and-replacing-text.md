---
aliases:
- /nl/java/searching-and-replacing-text/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:40.452854-07:00
description: "Zoeken en vervangen in Java overschrijft originele strings met nieuwe\
  \ karakters - zie het als een digitale correctievloeistof. Programmeurs gebruiken\
  \ dit\u2026"
lastmod: 2024-02-18 23:09:01.696452
model: gpt-4-0125-preview
summary: "Zoeken en vervangen in Java overschrijft originele strings met nieuwe karakters\
  \ - zie het als een digitale correctievloeistof. Programmeurs gebruiken dit\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?

Zoeken en vervangen in Java overschrijft originele strings met nieuwe karakters - zie het als een digitale correctievloeistof. Programmeurs gebruiken dit vaak om gegevens te schonen, instellingen aan te passen of berichten op maat te maken.

## Hoe te:

Zoeken en vervangen in Java is een fluitje van een cent dankzij de `String` klasse en zijn `replace()` methode. Zo doe je het:

```java
public class ReplaceDemo {
    public static void main(String[] args) {
        String origineleTekst = "De snelle bruine vos springt over de luie hond";
        String gewijzigdeTekst = origineleTekst.replace("luie", "energieke");
        
        System.out.println("Voor: " + origineleTekst);
        System.out.println("Na: " + gewijzigdeTekst);
    }
}
```

Output:
```
Voor: De snelle bruine vos springt over de luie hond
Na: De snelle bruine vos springt over de energieke hond
```

Nu, voor patronen of wildere vervangingen, komen `Pattern` en `Matcher` in het spel:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexReplaceDemo {
    public static void main(String[] args) {
        String origineleTekst = "Er zijn 31.536.000 seconden in 365 dagen.";
        Pattern patroon = Pattern.compile("\\d+");
        Matcher matcher = patroon.matcher(origineleTekst);
        String gewijzigdeTekst = matcher.replaceAll("#");
        
        System.out.println("Voor: " + origineleTekst);
        System.out.println("Na: " + gewijzigdeTekst);        
    }
}
```

Output:
```
Voor: Er zijn 31.536.000 seconden in 365 dagen.
Na: Er zijn # seconden in # dagen.
```

## Diepgaand:

De `replace()` methode vindt zijn oorsprong in de vroegste dagen van Java. Het maakt deel uit van de onveranderlijke `String` klasse, wat betekent dat elke keer dat je het gebruikt, je een nieuwe string creëert. Heel milieuvriendelijk, geen verspilling van het oude spul.

Maar wat is het verhaal met `Pattern` en `Matcher`, vraag je je af? Deze klassen maken deel uit van Java's reguliere expressie (regex) API, geïntroduceerd in Java 1.4. Ze voegen tanden toe aan zoeken en vervangen, waardoor je complexe patronen kunt detecteren en tekst dynamisch kunt aanpassen. Het is alsof je een scalpel gebruikt in plaats van een voorhamer.

Bovendien zijn er `replaceAll()` en `replaceFirst()`, twee methoden van de `Matcher` klasse die je teksttransformaties verfijnen, door alle voorkomens of slechts de eerste match te vervangen.

Een andere optie is het gebruik van de `StringBuffer` of `StringBuilder` klassen wanneer je te maken hebt met tonnen wijzigingen omdat, in tegenstelling tot `String`, deze buffers veranderlijk zijn.

## Zie ook:

- [Java String Documentatie](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Java Pattern Documentatie](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Matcher Documentatie](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Reguliere Expressies Tutorial](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)

Voor meer praktische oefening, bekijk RegexOne (https://regexone.com), het is een geweldige bron om je regex vaardigheden naar een hoger niveau te tillen.
