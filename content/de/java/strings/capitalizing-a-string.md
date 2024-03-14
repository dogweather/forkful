---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:34.897001-07:00
description: "Das Gro\xDFschreiben eines Strings bedeutet, den ersten Buchstaben jedes\
  \ Wortes im String in Gro\xDFbuchstaben umzuwandeln, w\xE4hrend die restlichen Buchstaben\u2026"
lastmod: '2024-03-13T22:44:53.746748-06:00'
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings bedeutet, den ersten Buchstaben jedes\
  \ Wortes im String in Gro\xDFbuchstaben umzuwandeln, w\xE4hrend die restlichen Buchstaben\u2026"
title: "Einen String gro\xDFschreiben"
---

{{< edit_this_page >}}

## Was und Warum?
Das Großschreiben eines Strings bedeutet, den ersten Buchstaben jedes Wortes im String in Großbuchstaben umzuwandeln, während die restlichen Buchstaben klein bleiben. Diese häufige Manipulation von Zeichenketten ist nützlich, um Text in Anwendungen zu formatieren, beispielsweise um Benutzernamen oder Titel gemäß Konvention oder grammatischer Richtigkeit darzustellen.

## Wie:
Die Standardbibliothek von Java bietet keine direkte Methode, um ganze Strings auf einmal großzuschreiben, aber Sie können dies mit einer Kombination von integrierten Methoden erreichen. Für anspruchsvollere Bedürfnisse bieten Drittanbieterbibliotheken wie Apache Commons Lang einfache Lösungen.

### Nutzung der integrierten Methoden von Java
Um einen String ohne externe Bibliotheken großzuschreiben, können Sie den String in Wörter aufteilen, den ersten Buchstaben jedes Wortes großschreiben und sie dann wieder zusammenfügen. Hier ist ein einfacher Ansatz:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // Ausgabe: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

Dieser Codeausschnitt wandelt den gesamten String in Kleinbuchstaben um und iteriert dann durch jeden Charakter, um den ersten Buchstaben jedes Wortes großzuschreiben. Er betrachtet Leerzeichen, Punkte und Apostrophe als Worttrenner.

### Nutzung von Apache Commons Lang

Die Bibliothek Apache Commons Lang bietet mit der Methode `WordUtils.capitalizeFully()` eine elegantere Lösung, die verschiedene Sonderfälle und Trennzeichen für Sie behandelt:

```java
// Abhängigkeit hinzufügen: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // Ausgabe: "Hello, World!"
    }
}
```

Um diese Methode zu verwenden, müssen Sie die Bibliothek Apache Commons Lang zu Ihrem Projekt hinzufügen. Diese Bibliotheksmethode macht nicht nur den ersten Buchstaben jedes Wortes groß, sondern wandelt auch die restlichen Buchstaben in jedem Wort in Kleinbuchstaben um und sorgt so für ein konsistentes Muster der Groß- und Kleinschreibung im ganzen String.
