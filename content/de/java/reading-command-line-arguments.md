---
title:    "Java: Lesen von Befehlszeilenargumenten"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

In vielen Java-Programmen ist es wichtig, Eingabe von Benutzern zu verarbeiten, z.B. beim Ausführen von Funktionen oder beim Übergeben von Werten an eine Anwendung. Eine praktische Methode, um dies zu erreichen, ist die Verwendung von Befehlszeilenargumenten. Diese ermöglichen es einem Benutzer, Werte oder Optionen beim Ausführen eines Programms anzugeben. Daher ist es wichtig zu wissen, wie man Befehlszeilenargumente in Java liest, um eine reibungslose Interaktion mit dem Benutzer zu gewährleisten.

## Anleitung

Um Befehlszeilenargumente in Java zu lesen, müssen wir zuerst die **args**-Parameter in der main-Methode verwenden. Diese Parameter enthalten ein Array von Strings, das die eingegebenen Argumente des Benutzers enthält. Wir können dann auf die einzelnen Argumente zugreifen, indem wir eine Schleife durch das Array erstellen oder auf bestimmte Indizes zugreifen, um spezifische Argumente zu erhalten.

Hier ist ein Beispielcode, der die Eingabe eines Benutzers bei der Ausführung eines Programms erwartet und das erste Argument zurückgibt:

```
public static void main(String[] args) {
    if (args.length > 0) {
        System.out.println("Das erste Argument lautet: " + args[0]);
    } else {
        System.out.println("Es wurden keine Argumente eingegeben.");
    }
}
```

Wenn der Benutzer beispielsweise "java MeinProgramm Hallo Welt" in der Befehlszeile eingibt, wird die Ausgabe "Das erste Argument lautet: Hallo" sein.

Beachten Sie, dass Befehlszeilenargumente immer als Strings übergeben werden, unabhängig davon, ob der Benutzer eine Zahl oder einen anderen Datentyp eingegeben hat. Daher müssen wir möglicherweise die Strings in den gewünschten Datentyp konvertieren, bevor wir sie in unserem Code verwenden.

## Vertiefung

Es ist auch möglich, Flags oder Optionen als Befehlszeilenargumente zu verwenden. Diese werden üblicherweise mit dem Präfix "-" oder "--" angegeben und können anschließend in unserem Code abgefragt werden. Zum Beispiel können wir die Option "-h" als Hilfe-Flag verwenden, um dem Benutzer Informationen über die verschiedenen möglichen Argumente zu geben.

Ein weiteres nützliches Feature ist die Verwendung von javadoc-Tags in unserem Code, um die wichtigsten Befehlszeilenargumente zu dokumentieren. Auf diese Weise kann der Benutzer einfach die Parameterliste in der Dokumentation des Programms überprüfen, um zu sehen, welche Argumente unterstützt werden und was sie bewirken.

## Siehe auch

- [Java-Befehlszeilenargumente](https://www.javatpoint.com/command-line-arguments-in-java)
- [Using Command-Line Arguments in Java](https://www.baeldung.com/java-command-line-arguments)
- [Creating Java Documentation with Javadoc](https://www.oracle.com/java/technologies/javase/javadoc-tool.html)