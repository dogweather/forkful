---
title:    "Java: Löschen von Zeichen, die einem Muster entsprechen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann bei der Bearbeitung von Daten oder Texten hilfreich sein. Es ermöglicht das einfache Entfernen von unerwünschten Zeichen oder das Anpassen von Formatierungen.

# Wie Es Geht

Im Folgenden wird gezeigt, wie man in Java Zeichen löschen kann, die einer bestimmten Regel entsprechen. Dafür werden zwei verschiedene Methoden vorgestellt.

### Methode 1: ReplaceAll()

Die erste Methode verwendet die `replaceAll()`-Funktion, die in Java Strings verfügbar ist. Diese Methode sucht nach einem bestimmten Muster in einem String und ersetzt alle passenden Zeichen durch ein leeres Zeichen (String ohne Inhalt). Dadurch werden die unerwünschten Zeichen gelöscht.

```Java
String text = "Die Sonne scheint, es ist ein schöner Tag!";
String bereinigterText = text.replaceAll("[,!]", "");
System.out.println(bereinigterText);
```

Output: "Die Sonne scheint es ist ein schöner Tag"

In diesem Beispiel wird die `replaceAll()`-Funktion verwendet, um alle Kommata und Ausrufezeichen aus dem Text zu entfernen. Durch die Nutzung von eckigen Klammern kann man angeben, welche Zeichen gelöscht werden sollen. Der leere String am Ende der Funktion signalisiert, dass diese Zeichen durch nichts ersetzt werden.

### Methode 2: Char Array

Die zweite Methode basiert auf der Umwandlung eines Strings in ein Char Array. Hierbei wird jeder Buchstabe oder jedes Zeichen des Strings einzeln überprüft und gegebenenfalls aus dem Array gelöscht.

```Java
String text = "Eine 1 und eine 2 gehen ins Kino!";
char[] textArray = text.toCharArray();

for (int i = 0; i < textArray.length; i++) {
    if (Character.isDigit(textArray[i])) {
        textArray[i] = ' ';
    }
}

text = String.valueOf(textArray);
System.out.println(text);
```

Output: "Eine und eine gehen ins Kino!"

In diesem Beispiel wird jedes Zeichen des Strings auf eine Ziffer überprüft und gegebenenfalls durch ein leeres Zeichen ersetzt. Hierfür wird die Funktion `isDigit()` aus der `Character`-Klasse verwendet.

# Tiefer Eintauchen

Beim Löschen von Zeichen sollte man darauf achten, dass die gewählte Methode auf die individuellen Anforderungen und Bedürfnisse angepasst wird. Die `replaceAll()`-Funktion ist in den meisten Fällen die einfachere und effizientere Wahl, da die Funktionalität bereits in Java integriert ist. Für spezifischere Fälle kann jedoch auch das Umwandeln in ein Char Array hilfreich sein.

# Siehe Auch

- [Java String Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [String.replaceAll() Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Character.isDigit() Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#isDigit-char-)