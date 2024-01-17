---
title:                "Die Länge eines Strings finden"
html_title:           "Java: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?
Die Länge einer Zeichenkette zu ermitteln bedeutet, die Anzahl der Zeichen in dieser Zeichenkette zu finden. Programmierer tun dies, um bestimmte Operationen auf Zeichenketten ausführen zu können, wie zum Beispiel die Überprüfung der Länge einer Benutzereingabe oder das Finden des längsten Wortes in einem Text.

# So geht's:
```Java
String str = "Hallo Welt";
int length = str.length();
System.out.println(length);
```
Output: 11

# Eintauchen:
Die Notwendigkeit der Ermittlung der Zeichenkettenlänge geht bis in die Anfänge der Programmierung zurück, als der Zugriff auf das Ende einer Zeichenkette aufgrund begrenzter Ressourcen kompliziert war. Eine alternative Methode zur Bestimmung der Länge einer Zeichenkette ist die Verwendung einer Schleife, aber die Methodenlänge bietet eine einfachere und effizientere Lösung. Die Implementation kann je nach Programmiersprache variieren, aber im Allgemeinen zählt man einfach die Anzahl der Zeichen und gibt diese zurück.

# Siehe auch:
Weitere Informationen zur Länge von Zeichenketten in Java finden Sie in der offiziellen Java-Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length()