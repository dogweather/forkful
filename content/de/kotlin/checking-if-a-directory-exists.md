---
title:                "Kotlin: Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt beim Programmieren in Kotlin. Es kann verwendet werden, um sicherzustellen, dass Dateien an den richtigen Orten vorhanden sind oder um Fehler zu vermeiden, die durch das Fehlen eines bestimmten Verzeichnisses verursacht werden können.

## Wie es geht

Das Überprüfen der Existenz eines Verzeichnisses kann sehr einfach sein. Dazu kann die bereits in Kotlin integrierte Funktion `File.exists()` verwendet werden. Diese Funktion gibt zurück, ob das angegebene Verzeichnis existiert oder nicht. Hier ist ein Beispielcode, der die Funktion zeigt:

```Kotlin
val directory = File("Pfad/zum/Verzeichnis")
if(directory.exists()){
    println("Das Verzeichnis existiert.")
} else {
    println("Das Verzeichnis existiert nicht.")
}
```

Die Ausgabe wird je nachdem, ob das Verzeichnis vorhanden ist oder nicht, unterschiedlich sein. Wenn das Verzeichnis existiert, wird die erste Ausgabe erscheinen, sonst die zweite.

## Tiefergehende Informationen

Bei der Überprüfung der Existenz eines Verzeichnisses gibt es einige Dinge zu beachten. Zum Beispiel kann das Verzeichnis, das Sie überprüfen, sowohl eine Datei als auch ein Verzeichnis sein. In diesem Fall gibt die Methode `exists()` trotzdem `true` zurück, da es sich um ein gültiges Verzeichnis handelt.

Es ist auch wichtig zu beachten, dass diese Methode nur die Existenz eines Verzeichnisses oder einer Datei auf dem aktuellen Dateisystem überprüfen kann. Wenn Sie auf Verzeichnisse oder Dateien auf einem externen Dateisystem zugreifen möchten, müssen Sie spezielle Funktionen verwenden.

## Siehe auch

- [Kotlin Dokumentation - Überprüfen der Existenz von Dateien und Verzeichnissen](https://kotlinlang.org/docs/tutorials/io.html#checking-the-existence-of-a-file-or-directory)
- [Tutorialspoint - Kotlin: Überprüfen der Existenz von Verzeichnissen und Dateien](https://www.tutorialspoint.com/kotlin/kotlin_checking_the_existence_of_directories_and_files.htm)
- [Baeldung - Genaues Überprüfen von Dateien und Verzeichnissen in Kotlin](https://www.baeldung.com/kotlin/check-file-directory-exists)