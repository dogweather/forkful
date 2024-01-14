---
title:    "Java: Einen Textordner lesen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit in der Java-Programmierung. Mit dieser Fähigkeit können wir Daten von externen Quellen in unser Programm importieren und verarbeiten. Es ist daher wichtig zu wissen, wie man richtig Textdateien liest, um effiziente und leistungsstarke Programme zu schreiben.

## Wie geht man vor

Um mit Java eine Textdatei zu lesen, müssen wir zunächst eine Instanz der `File`-Klasse erstellen, die die Datei repräsentiert. Dann erstellen wir eine Instanz der `Scanner`-Klasse, die es uns ermöglicht, die Datei Zeile für Zeile zu durchlaufen und die Daten zu lesen. Wir nutzen die `File`-Instanz als Parameter für den Konstruktor der `Scanner`-Klasse, um die Datei zu öffnen. Anschließend können wir die `nextLine()`-Methode der `Scanner`-Klasse verwenden, um jede Zeile der Datei zu lesen und die Daten zu verarbeiten.

```Java
File file = new File("textdatei.txt"); // Dateipfad anpassen
Scanner scanner = new Scanner(file);

while (scanner.hasNextLine()) { // liest jede Zeile der Datei
    String line = scanner.nextLine();
    System.out.println(line); // gibt die Zeile auf der Konsole aus
}

scanner.close(); // schließen der Datei
```

Die obige Beispiel zeigt, wie wir eine Textdatei Zeile für Zeile lesen und die Daten auf der Konsole ausgeben können.

## Tiefer Einblick

Beim Lesen einer Textdatei ist es wichtig zu beachten, welche Codierung die Datei hat. Standardmäßig verwendet Java die Systemcodierung, um Textdateien zu lesen. Wenn die Datei jedoch eine andere Codierung hat, muss dies beim Öffnen der Datei durch die Verwendung des richtigen Konstruktors der `Scanner`-Klasse angegeben werden. Zum Beispiel, wenn die Datei im UTF-8-Format ist, müssen wir den `Scanner`-Konstruktor mit der Zeichenkette "UTF-8" als zweites Argument aufrufen.

```Java
File file = new File("textdatei.txt");
Scanner scanner = new Scanner(file, "UTF-8"); // Angabe der Codierung

while (scanner.hasNextLine()) {
    String line = scanner.nextLine();
    System.out.println(line);
}

scanner.close();
```

Außerdem ist zu beachten, dass wir während des Lesens der Datei möglicherweise auf Exceptions stoßen können, wie z.B. die `FileNotFoundException`. Daher ist es wichtig, diese Exceptions zu handhaben und entsprechende Fehlerbehandlung zu implementieren.

## Siehe auch

- [Java File API Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java Scanner Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)