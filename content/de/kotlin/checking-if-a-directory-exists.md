---
title:    "Kotlin: Überprüfung der Existenz eines Verzeichnisses"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

##Warum

Das Überprüfen ob ein Verzeichnis existiert, ist eine wichtige Aufgabe in der Programmierung, um sicherzustellen, dass der Code reibungslos ausgeführt wird und keine Fehler verursacht. Es ist also sinnvoll, zu wissen, wie man dies in Kotlin erreichen kann.

##Wie geht das

Es ist relativ einfach, in Kotlin zu überprüfen, ob ein Verzeichnis existiert. Hier ist ein Beispielcode:
```Kotlin
val directory = File("C:/Users/Username/Documents")

if(directory.exists()){
    println("Das Verzeichnis existiert.")
} else{
    println("Das Verzeichnis existiert nicht.")
}
```
Dieser Code erstellt eine `File` Instanz für das angegebene Verzeichnis und verwendet dann die Methode `exists()`, um zu prüfen, ob es tatsächlich existiert oder nicht. Je nach Ergebnis wird eine entsprechende Nachricht ausgegeben.

##Tiefergehende Informationen

Bevor Sie beginnen, Verzeichnisse in Ihrem Code zu überprüfen, sollten Sie verstehen, wie sie in Ihrem Betriebssystem strukturiert sind. In Windows werden Verzeichnisse normalerweise durch Verzeichnisnamen und dem Backslash-Symbol (`\`) getrennt, während in Unix-Systemen der Trennzeichen erfolgt durch das Vorwärtsschrägstrich-Symbol (`/`).

Es gibt auch die Möglichkeit, in Kotlin mit relativen Pfaden zu arbeiten, indem man die Methode `relativeTo()` verwendet. In Linux-Systemen können beispielsweise relative Pfade von Ihrem aktuellen Arbeitsverzeichnis aus in einem Befehl verwendet werden. Dies ist besonders nützlich, wenn Sie Code schreiben, der auf verschiedenen Systemen ausgeführt werden soll.

##Siehe auch

- [Dokumentation zu `File` in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Tutorial zu Datei- und Verzeichnisoperationen in Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_file_io.htm)
- [Vergleich von Windows- und Unix-Pfaden](https://www.shell-tips.com/2010/06/13/file-paths-in-windows-and-unix/)