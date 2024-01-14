---
title:    "Java: Schreiben in die Standardfehlerausgabe"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben in den Standardfehler oder die System Fehlerausgabe (stderr) ist ein wichtiger Aspekt der Java-Programmierung. Es ermöglicht uns, Fehler und Debugging-Informationen zu erfassen und anzuzeigen, die für die Entwicklung und Fehlerbehebung unserer Anwendungen entscheidend sein können.

## Wie geht's

Um in den Standardfehler zu schreiben, müssen wir zuerst die Klasse "System" verwenden und die Methode "err" aufrufen. Dann können wir die Methode "println" verwenden, um unseren gewünschten Text auszugeben.

```Java
System.err.println("Dieser Text wird in den Standardfehler geschrieben.");
```

Die Ausgabe erscheint dann rot in der Konsole und wird immer angezeigt, auch wenn die Standardausgabe (stdout) umgeleitet wird.

## Tiefer Tauchen

Im Hintergrund nutzt Java die sogenannte "System.err" PrintStream-Instanz, um Ausgaben in den Standardfehler zu schreiben. Diese kann auch durch die Verwendung der "setErr()" Methode geändert werden, um z.B. auf eine Datei oder einen Logger umzuleiten.

Es ist auch wichtig zu beachten, dass Schreiboperationen in den Standardfehler teurer sind als in die Standardausgabe, da sie durch das System synchronisiert werden. Aus diesem Grund sollten wir sie nur für besonders wichtige oder fehlerhafte Nachrichten verwenden.

## Siehe auch

- [Java Dokumentation zu der "System" Klasse](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Tutorial zu Standard Streams in Java](https://www.baeldung.com/java-standard-Streams)
- [Artikel über das Schreiben in den Standardfehler in Java](https://stackify.com/java-system-out-println-vs-system-err-println/)