---
title:    "Java: Großschreibung einer Zeichenfolge"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Warum

Das Verwenden von Großbuchstaben in Strings kann nützlich sein, wenn man möchte, dass bestimmte Wörter oder Sätze hervorgehoben werden. Es kann auch bei der Validierung von Eingaben hilfreich sein, um sicherzustellen, dass sie im richtigen Format sind.

# Wie geht man vor

```Java
public class Main {
  public static void main(String[] args) {
    // Beispiel für die Verwendung von toUpperCase()
    String name = "Max Mustermann";
    String capitalizedName = name.toUpperCase();
    System.out.println(capitalizedName);
    // Output: MAX MUSTERMANN
  }
}
```

In der obigen Code-Schnipsel sehen wir die Verwendung der `toUpperCase()` Methode, die in Java vorhanden ist. Diese Methode konvertiert alle Buchstaben eines Strings in Großbuchstaben. Sie kann auf alle Strings angewendet werden, unabhängig von ihrer Länge.

Es gibt auch die `toLowerCase()` Methode, die das Gegenteil bewirkt und alle Buchstaben in Kleinbuchstaben umwandelt. Diese Methode kann ebenfalls für die Formatierung und Validierung von Eingaben verwendet werden.

# Tiefer tauchen

Es ist wichtig zu beachten, dass die `toUpperCase()` und `toLowerCase()` Methoden die ursprünglichen Strings nicht ändern, sondern lediglich eine neue, formatierte Version zurückgeben. Daher ist es wichtig, die Rückgabe einer Variablen zuzuweisen, wenn man die formatierten Strings in Zukunft verwenden möchte.

Man kann die `toUpperCase()` und `toLowerCase()` Methoden auch mit Bedingungen, Schleifen und anderen Methoden kombinieren, um komplexe Formatierungen und Validierungen von Strings durchzuführen.

# Siehe auch
- Java String Klasse Dokumentation: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Java String Methoden: https://www.w3schools.com/java/java_ref_string.asp