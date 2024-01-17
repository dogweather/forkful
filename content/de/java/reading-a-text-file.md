---
title:                "Eine Textdatei lesen"
html_title:           "Java: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

Was ist das Lesen einer Textdatei und warum tun Programmierer es?
Das Lesen einer Textdatei ist ein Prozess, bei dem ein Computer einen Text aus einer Datei ausliest und in einem Programm verwendet. Programmierer tun dies, um Informationen aus einer Datei zu erhalten und sie in ihrem Code zu verwenden, anstatt hartcodierte Werte zu verwenden.

Wie geht es?
Java bietet verschiedene Möglichkeiten zum Lesen von Textdateien. Eine Möglichkeit ist die Verwendung der FileReader-Klasse, um eine Datei zu öffnen und anschließend die BufferedReader-Klasse zum Lesen der tatsächlichen Datei zu verwenden. Hier ist ein Beispiel:

```Java
FileReader reader = new FileReader("meineDatei.txt");
BufferedReader br = new BufferedReader(reader);

String line;
while((line = br.readLine()) != null) {
    System.out.println(line);
}

br.close();
```

Die obige Methode liest jede Zeile aus der Datei und druckt sie in der Konsole aus. Eine andere Möglichkeit besteht darin, die Scanner-Klasse zu verwenden, um die Datei zu lesen. Hier ist ein Beispiel:

```Java
Scanner scanner = new Scanner(new File("meineDatei.txt"));

while(scanner.hasNextLine()) {
    System.out.println(scanner.nextLine());
}

scanner.close();
```

Diese Methode liest auch jede Zeile aus der Datei und gibt sie in der Konsole aus. Weitere Details zu den einzelnen Klassen und Methoden finden Sie in der Java-Dokumentation.

Tief Ins Detail Gehen 
Das Lesen von Textdateien hat eine lange Geschichte in der Programmierung und wird immer noch häufig verwendet, auch wenn es alternative Methoden wie das Lesen von Datenbanken gibt. Es ist wichtig zu beachten, dass der beim Lesen von Dateien verwendete Pfad von der Umgebung abhängig ist und möglicherweise angepasst werden muss, um in verschiedenen Betriebssystemen zu funktionieren.

Siehe auch 
Hier sind einige nützliche Links, die weitere Informationen zum Lesen von Textdateien in Java enthalten: 
- [Java-Dokumentation für FileReader](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [Java-Dokumentation für BufferedReader](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html) 
- [Java-Dokumentation für Scanner](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)