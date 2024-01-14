---
title:                "Java: Lesen von Befehlszeilenargumenten"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

In der Welt der Java Programmierung, kann es manchmal verwirrend sein, einige Konzepte und Techniken zu verstehen. Eine dieser Techniken ist das Lesen von Befehlszeilenargumenten. Doch warum sollten Sie sich damit beschäftigen? Einfach ausgedrückt, das Lesen von Befehlszeilenargumenten ermöglicht es Ihnen, Benutzereingaben zu akzeptieren und Ihr Programm entsprechend anzupassen. Es ist eine großartige Möglichkeit, Interaktivität in Ihre Anwendungen zu bringen und sie an verschiedene Bedürfnisse anzupassen. Aber wie genau macht man das? Lesen Sie weiter, um es herauszufinden!

# Wie geht das?

Wie bei jeder Programmiersprache gibt es mehrere Wege, Befehlszeilenargumente in Java zu lesen. Wir werden hier die einfachste Methode betrachten, die "args" Methode. Diese Methode ist Teil der "main" Methode und wird automatisch ausgeführt, wenn das Programm gestartet wird. Um die Befehlszeilenargumente zu lesen, müssen Sie nur die "args" Variable in Ihrem Code verwenden. Ein einfaches Beispiel sieht wie folgt aus:

```Java
public class CommandLineArgsExample {
    public static void main(String[] args) {
        for (int i = 0; i < args.length; i++) {
            System.out.println("Argument Nummer " + i + ": " + args[i]);
        }
    }    
}
```
Mit diesem Code können Sie alle Befehlszeilenargumente ausgeben und deren Index anzeigen. Wenn Sie nun Ihr Programm ausführen und Befehlszeilenargumente angeben, werden diese zusammen mit dem entsprechenden Index in der Konsole ausgegeben. Zum Beispiel:

```Java
java CommandLineArgsExample "Hallo" "Welt" "!" 
```

Ausgabe: 
Argument Nummer 0: Hallo
Argument Nummer 1: Welt
Argument Nummer 2: !

# Tiefergehende Informationen

Nun, da Sie wissen, wie man Befehlszeilenargumente liest, gibt es noch einige weitere Details, die Sie beachten sollten. Zum Beispiel können Sie mithilfe von "String[] args" verschiedene Datentypen lesen, nicht nur Strings. Außerdem können Sie auch Optionen für die Befehlszeilenargumente definieren, um bestimmte Eingaben zu verlangen oder eine Standardoption festzulegen. Es gibt auch verschiedene Bibliotheken und Frameworks, die Ihnen helfen können, Befehlszeilenargumente einfacher zu verarbeiten. Auch hier ist die Erweiterungsmöglichkeit in Ihrem Code endlos. Am besten machen Sie sich mit der offiziellen Java Dokumentation vertraut, um alle Möglichkeiten zu erkunden.

# Siehe auch

- [Oracle Offizielle Java Dokumentation](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Baeldung - Guide zu Befehlszeilenargumenten in Java](https://www.baeldung.com/java-command-line-arguments)
- [DZone - Befehlszeilenargumente verstehen](https://dzone.com/articles/java-command-line-arguments-how-to-parse-and-use-th)