---
date: 2024-01-20 17:58:16.283929-07:00
description: 'How to: Java macht Textersetzungen ziemlich einfach mit der `String`
  Klasse.'
lastmod: '2024-03-13T22:44:53.748738-06:00'
model: gpt-4-1106-preview
summary: Java macht Textersetzungen ziemlich einfach mit der `String` Klasse.
title: Suchen und Ersetzen von Text
weight: 10
---

## How to:
Java macht Textersetzungen ziemlich einfach mit der `String` Klasse.

```java
public class TextReplaceBeispiel {
    public static void main(String[] args) {
        String originalText = "Hier ist der Originale Text. Originale Texte sind gut.";
        String ersetzterText = originalText.replace("Originale", "Veränderte");
        System.out.println(ersetzterText);
    }
}
```

Erwartete Ausgabe:

```
Hier ist der Veränderte Text. Veränderte Texte sind gut.
```

Für komplexere Suchen kann `Pattern` und `Matcher` genutzt werden – das sind Teile der `java.util.regex` Bibliothek:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexBeispiel {
    public static void main(String[] args) {
        String originalText = "Foo123Bar";
        Pattern muster = Pattern.compile("\\d+");
        Matcher matcher = muster.matcher(originalText);
        String ersetzterText = matcher.replaceAll("#");
        System.out.println(ersetzterText);
    }
}
```

Erwartete Ausgabe:

```
Foo###Bar
```

## Deep Dive
Suchen und Ersetzen gibt’s schon ewig - seit den Anfängen des Computings. Der einfache `.replace()` ist schnell und effektiv für Direktsubstitutionen. Aber wenn's um Mustererkennung geht, bringen reguläre Ausdrücke – kurz RegExp – ihre Muskeln ins Spiel.

RegExp kann anfangs verwirrend sein, aber sie sind leistungsstark. Sie ermöglichen detaillierte Suchmuster und Bedingungen. `java.util.regex` ist die Standardbibliothek dafür in Java.

Es gibt Alternativen zu `java.util.regex` wie Apache's `StringUtils` oder `Replacer` aus Google's Guava-Bibliothek. Diese können einfacher sein oder spezielle Fälle besser handhaben.

Beim Implementieren ist es wichtig, auf Performance zu achten. RegExp kann langsam sein, besonders bei großen Texten. Merke dir: je einfacher das Muster, desto schneller die Suche.

## See Also
- [Oracle's JavaDoc on Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
- [Google Guava Libraries](https://github.com/google/guava)
