---
title:                "Einen String großschreiben"
html_title:           "Java: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings bedeutet, jeden Buchstaben des Strings in Großbuchstaben umzuwandeln. Programmierer tun dies, um Text hervorzuheben oder ein einheitliches Format zu gewährleisten.

## So geht's:

Mit Java können Sie einen String ganz einfach mit der Methode `toUpperCase()` in Großbuchstaben umwandeln. Hier ist ein Beispiel:

```Java
public class Hauptprogramm {
    public static void main(String[] args) {
        String kleinerText = "das ist ein kleiner text";
        String grosserText = kleinerText.toUpperCase();

        System.out.println(grosserText);  //"DAS IST EIN KLEINER TEXT"
    }
}
```
Die Ausgabe dieses Codes ist "DAS IST EIN KLEINER TEXT".

## Vertiefung:

Geschichte: Die Methode `toUpperCase()` existiert seit der ersten Version von Java und ist eine der am häufigsten verwendeten Methoden beim Arbeiten mit Strings.

Alternativen: Sie können auch die Methode `Locale.ROOT` verwenden, um sicherzustellen, dass die Großschreibung unabhängig von der Landeseinstellung des Benutzers korrekt erfolgt:

```Java
String grosserText = kleinerText.toUpperCase(Locale.ROOT);
```

Implementierung: Unter der Haube verwendet Java einen speziellen Algorithmus, um jeden Kleinbuchstaben im String in einen Großbuchstaben umzuwandeln. Dies wird durch die Anwendung von Unicode-Werten erreicht.

## Siehe auch:

Um mehr über die Arbeit mit Strings in Java zu erfahren, sehen Sie sich die Java-Dokumentation an: 
- [Java String-Klasse](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java-Dokumentation für Locale](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html).