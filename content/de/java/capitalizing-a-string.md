---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings bedeutet, alle Buchstaben darin in Großbuchstaben umzuwandeln. Programmierer nutzen das, um Konsistenz in Nutzereingaben zu gewährleisten oder um Bedeutungen hervorzuheben.

## Wie geht das?
Java macht das Kapitalisieren dank der `toUpperCase()`-Methode der `String`-Klasse einfach. Hier ein kurzes Beispiel:

```java
public class CapitalizeExample {
    public static void main(String[] args) {
        String lowerCaseString = "das ist ein beispiel.";
        String capitalizedString = lowerCaseString.toUpperCase();

        System.out.println(capitalizedString); // Output: DAS IST EIN BEISPIEL.
    }
}
```
Ruf einfach `toUpperCase()` auf und dein String erstrahlt in Großbuchstaben – so simpel ist das.

## Tiefgehende Einblicke
Historisch gesehen existieren Großbuchstaben seit den Tagen der Römischen Reiches. In digitalen Zeiten nutzen wir Kapitalisierung aus praktischen Gründen: Suchoperationen, Textvergleiche oder Gestaltung von Benutzeroberflächen. Andererseits gibt es Alternativen wie die `toLowerCase()`-Methode, wenn man alles klein haben möchte. Und wenn du spezielle Regeln für die Kapitalisierung, wie beispielsweise Titel in einem Buch, umsetzen möchtest, brauchst du speziellere Methoden als `toUpperCase()`. Dabei kann es knifflig werden, da Sprachen wie Deutsch mit Umlauten oder dem ß arbeiten, die besondere Regeln bei der Umwandlung in Großbuchstaben haben.

Du solltest wissen, dass `toUpperCase()` die Groß- und Kleinschreibung gemäß den Regeln der Standardlokalität (Standard locale) des Systems umwandelt. In manchen Fällen willst du vielleicht eine spezifische Locale verwenden, um sicherzustellen, dass deine Umwandlung kulturellen und sprachlichen Standards entspricht:

```java
String lowerCaseStringWithUmlaut = "straße";
String capitalizedStringWithUmlaut = lowerCaseStringWithUmlaut.toUpperCase(Locale.GERMANY); // Output: STRASSE
```

Beachte, dass `ß` zu `SS` in Großbuchstaben mit der deutschen Locale wird.

## Siehe auch
- [Java String Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Locale Class in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
- [Understanding Java String](https://www.baeldung.com/java-string)
  
Mit diesen Links kannst du tiefer in die Welt der Java-Strings eintauchen. Frohes Codieren!
