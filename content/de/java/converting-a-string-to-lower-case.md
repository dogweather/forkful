---
title:    "Java: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum es sinnvoll sein könnte, einen String in Java in Kleinbuchstaben umzuwandeln. Zum Beispiel kann es bei der Suche oder der Vergleichung von Strings helfen, wenn alle Buchstaben klein geschrieben sind. Auch bei der Eingabe von Benutzernamen oder Passwörtern kann es nützlich sein, diese in Kleinbuchstaben zu konvertieren, um mögliche Fehler zu vermeiden.

## Wie geht das?

Um einen String in Java in Kleinbuchstaben umzuwandeln, gibt es verschiedene Möglichkeiten. Die einfachste ist die Verwendung der eingebauten Methode `toLowerCase()`, die für jede String-Instanz verfügbar ist. Hier ist ein Beispielcode:

```Java
String input = "Hallo";
String output = input.toLowerCase();
System.out.println(output); // gibt "hallo" aus
```

Man kann auch die `toLowerCase()` Methode mit einer bestimmten `Locale` verwenden, um sicherzustellen, dass die Umwandlung nach den Regeln dieser Sprache erfolgt. Hier ist ein Beispiel mit der deutschen `Locale`:

```Java
String input = "Guten Morgen";
String output = input.toLowerCase(Locale.GERMAN);
System.out.println(output); // gibt "guten morgen" aus
```

Eine andere Möglichkeit ist die Verwendung der `String` Klasse `toLowerCase()` Methode, die optional eine `Locale` als Parameter akzeptiert. Hier ist ein Beispiel:

```Java
String input = "Ich heiße Max";
String output = input.toLowerCase(Locale.GERMAN);
System.out.println(output); // gibt "ich heiße max" aus
```

## Tiefere Einblicke

Bei der Verwendung der `toLowerCase()` Methode ist es wichtig zu beachten, dass sie eine neue `String` Instanz zurückgibt und den ursprünglichen `String` nicht ändert. Dies liegt daran, dass `String` Objekte in Java unveränderlich sind. Wenn man jedoch eine `String` Variable hat, die auf den ursprünglichen `String` zeigt, und den Wert ändern möchte, muss man sie auf die neue, konvertierte Instanz setzen.

Ein weiteres wichtiges Detail ist, dass die `toLowerCase()` Methode, je nach `Locale`, unterschiedliche Ergebnisse erzeugen kann. Zum Beispiel gibt es im Deutschen die Buchstaben "ß" und "ss". In der deutschen `Locale` wird "ß" beim Umwandeln in Kleinbuchstaben zu "ss", während es in anderen Standard-`Locale`s möglicherweise nicht der Fall ist.

## Siehe auch

- [Java Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java String Dokumentation](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html)
- [Java Locale](https://docs.oracle.com/javase/10/docs/api/java/util/Locale.html)