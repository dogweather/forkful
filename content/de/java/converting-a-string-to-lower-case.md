---
title:                "Java: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
 
Das Konvertieren von einer Zeichenkette in Kleinbuchstaben ist eine häufige Aufgabe beim Programmieren. Indem man eine Zeichenkette zu Lower Case umwandelt, können wir sicherstellen, dass die Groß- und Kleinschreibung bei der Verarbeitung von Benutzereingaben oder Daten konsistent bleibt. Es ist auch hilfreich, wenn wir überprüfen möchten, ob zwei Zeichenketten gleich sind, unabhängig von der Groß- und Kleinschreibung.
 
## Wie
 
Das Umwandeln einer Zeichenkette in Kleinbuchstaben ist in Java einfach und kann auf verschiedene Weisen erfolgen. Hier sind zwei Beispiele:
 
```Java
// Beispiel 1: Verwendung der toLowerCase()-Methode
String str1 = "Hallo";
String str2 = str1.toLowerCase();
System.out.println(str2); // Gibt "hallo" aus
```
 
```Java
// Beispiel 2: Verwendung von String.toLowerCase(Locale) für spezifische Locale
String str3 = "GuteN Tag";
String str4 = str3.toLowerCase(Locale.GERMAN);
System.out.println(str4); // Gibt "guten tag" aus
```
 
Beide Methoden geben eine neue Zeichenkette zurück und verändern nicht die ursprüngliche Zeichenkette.
 
## Deep Dive
 
Das Konvertieren einer String in Lower Case mag einfach erscheinen, aber es gibt einige wichtige Dinge zu beachten. Zum Beispiel gibt es in manchen Sprachen, wie dem Türkischen, Buchstaben mit Doppelakut (˝) oder Cedille (¸), die beim Umwandeln in Kleinbuchstaben ihre Form verändern können.
 
Außerdem müssen wir bei der Verwendung von toLowerCase() bedenken, dass dies für ASCII- und Unicode-Zeichen unterschiedlich funktioniert. ASCII-Zeichen werden einfach in Kleinbuchstaben umgewandelt, während Unicode-Zeichen unter Umständen mehrere Code-Points enthalten können und die Groß- und Kleinschreibung auf komplexere Weise verarbeiten werden muss.
 
Um diese Komplexität zu umgehen, können wir die klare und gut getestete String.toLowerCase(Locale) Methode verwenden, die spezifische Sprachen und Länder berücksichtigt.
 
## Siehe auch
 
- Java String Dokumentation (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- Java Locale Dokumentation (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Locale.html)
- Java String.toLowerCase(Locale) Beispiel (https://www.baeldung.com/java-lowercase-string)