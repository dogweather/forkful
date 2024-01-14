---
title:    "Java: Lesen von Befehlszeilenargumenten"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit in der Java-Programmierung. Es ermöglicht es Entwicklern, ihre Programme mit unterschiedlichen Parametern auszuführen und so flexibler zu gestalten.

## Wie man es macht

Das Lesen von Befehlszeilenargumenten ist einfach und kann mit nur wenigen Zeilen Java-Code erreicht werden. Zuerst müssen Sie die `args`-Variable in Ihrer `main`-Methode deklarieren und dann können Sie darauf zugreifen, um die übergebenen Argumente zu erhalten. Ein Beispiel sieht folgendermaßen aus:

```Java
public static void main(String[] args) {
    System.out.println("Das erste Argument ist: " + args[0]);
    System.out.println("Das zweite Argument ist: " + args[1]);
}
```

Wenn Sie dieses Programm mit den Befehlszeilenargumenten "Hallo" und "Welt" ausführen, sollte die Ausgabe folgendermaßen aussehen:

```Java
Das erste Argument ist: Hallo
Das zweite Argument ist: Welt
```

Sie können auch überprüfen, wie viele Argumente übergeben wurden, indem Sie auf die Länge der `args`-Variable zugreifen. Hier ist ein weiteres Beispiel:

```Java
public static void main(String[] args) {
    if (args.length < 3) {
        System.out.println("Es wurden nicht genügend Argumente übergeben.");
    } else {
        System.out.println("Das dritte Argument ist: " + args[2]);
    }
}
```

## Tiefer ins Detail gehen

Obwohl das Lesen von Befehlszeilenargumenten einfach ist, gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen die Argumente korrekt in der richtigen Reihenfolge übergeben werden, um darauf zugreifen zu können. Auch ist es wichtig, die Länge der `args`-Variable zu überprüfen, um sicherzustellen, dass ausreichend Argumente übergeben wurden. Es gibt auch Möglichkeit, benannte Argumente zu verwenden, aber das ist etwas fortgeschrittener.

## Siehe auch

- Java Tutorials: Lesen von Befehlszeilenargumenten (https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Stack Overflow: Wie man Befehlszeilenargumente in Java liest (https://stackoverflow.com/questions/890966/how-to-read-command-line-arguments)