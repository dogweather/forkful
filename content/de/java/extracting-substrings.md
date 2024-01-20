---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilstrings ist, der Prozess, Teile eines Strings basierend auf bestimmten Parametern wie Start- und Endindex zu isolieren. Programmierer tun dies häufig, um spezifische Informationen aus größeren Textdatensätzen zu erfassen oder Strings zu manipulieren.

## So geht's:

Dank der Methode `substring()` von Java ist das Extrahieren von Teilstrings eher unkompliziert. Ein Beispiel:

```Java
public class Main {
  public static void main(String[] args) {
    String str = "Hallo, Welt!";
    String extract = str.substring(7, 12);
    System.out.println(extract);
  }
}
```

Die Ausgabe wäre: `Welt`

## Vertiefung:

Das Extrahieren von Teilstrings basiert auf Methoden, die in den frühsten Versionen von Java eingeführt wurden. Als Alternative zur Methode `substring()`, kann auch die Methode `split()` genutzt werden, insbesondere wenn man einen String in mehrere Teilstrings auf der Grundlage eines bestimmten Trennzeichens aufteilen möchte. Hinsichtlich der Implementierungsdetails erfolgt die Extraktion intern durch das Erzeugen eines neuen String-Objekts, das die extrahierten Zeichen enthält. Es ist effizient, da keine neuen Zeichen erstellt werden. Stattdessen wird ein Zeiger auf das ursprüngliche Zeichenarray verwendet.

## Siehe Auch:

- Oracle Java SE Dokumentation: [Strings](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- Java-Buch: [Strings und Zeichenketten](http://openbook.rheinwerk-verlag.de/javainsel/06_003.html#u6.2)
- Stack Overflow Diskussion: [substring vs split](https://stackoverflow.com/questions/3651485/what-is-more-efficient-splitting-a-string-with-split-or-with-substring-in-java)