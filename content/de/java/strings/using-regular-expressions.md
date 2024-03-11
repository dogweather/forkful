---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:15.471744-07:00
description: "Regul\xE4re Ausdr\xFCcke (regex) in Java erm\xF6glichen es Ihnen, spezifische\
  \ Muster zu definieren, um Strings in Ihrem Code zu suchen, zu manipulieren oder\
  \ zu\u2026"
lastmod: '2024-03-11T00:14:27.645279-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke (regex) in Java erm\xF6glichen es Ihnen, spezifische\
  \ Muster zu definieren, um Strings in Ihrem Code zu suchen, zu manipulieren oder\
  \ zu\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
---

{{< edit_this_page >}}

## Was & Warum?

Reguläre Ausdrücke (regex) in Java ermöglichen es Ihnen, spezifische Muster zu definieren, um Strings in Ihrem Code zu suchen, zu manipulieren oder zu validieren. Programmierer nutzen sie für Aufgaben wie das Parsen von Logdateien, das Validieren von Benutzereingaben oder die Suche nach spezifischen Mustern in Texten, was eine ausgefeilte Stringverarbeitung mit minimalem Aufwand ermöglicht.

## Wie geht das:

Die integrierte Unterstützung für regex in Java erfolgt hauptsächlich durch die Klassen `Pattern` und `Matcher` im Paket `java.util.regex`. Hier ist ein einfaches Beispiel, um alle Vorkommen eines Wortes in einem String zu finden und auszudrucken, unabhängig von Groß- und Kleinschreibung:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex ist großartig zum Parsen. Parsen mit regex ist mächtig.";
        String wordToFind = "parsen";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Gefunden '" + matcher.group() + "' an Position " + matcher.start());
        }
    }
}
```

Ausgabe:
```
Gefunden 'parsen' an Position 16
Gefunden 'Parsen' an Position 31
```

Für Aufgaben wie das Aufteilen von Strings können Sie die `split()` Methode der `String`-Klasse mit einem regex verwenden:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

Ausgabe:
```
Java
Python
Ruby
JavaScript
```

Bei der Arbeit mit regex in Java kann es Fälle geben, in denen eine externe Bibliothek komplexe Aufgaben vereinfachen kann. Eine der beliebten Drittanbieter-Bibliotheken für die Arbeit mit regex in Java ist `Apache Commons Lang`. Sie bietet Hilfsprogramme wie `StringUtils`, die einige regex-Aufgaben vereinfachen. So nutzen Sie es, um die Übereinstimmungen eines Substrings zu zählen:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex macht die Textverarbeitung einfacher. Textverarbeitung mit regex ist effizient.";
        String substring = "Textverarbeitung";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' erscheint " + count + " mal.");
    }
}
```

Um Apache Commons Lang zu nutzen, müssen Sie es in Ihr Projekt einbinden. Wenn Sie Maven verwenden, fügen Sie diese Abhängigkeit zu Ihrer `pom.xml` hinzu:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- Prüfen Sie auf die neueste Version -->
</dependency>
```

Ausgabe:
```
'Textverarbeitung' erscheint 2 mal.
```
