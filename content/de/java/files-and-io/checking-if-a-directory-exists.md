---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:59.024536-07:00
description: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Java existiert, ist eine\
  \ grundlegende Aufgabe, die das Verifizieren der Pr\xE4senz eines Dateisystemverzeichnisses\
  \ vor\u2026"
lastmod: '2024-03-13T22:44:53.776677-06:00'
model: gpt-4-0125-preview
summary: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Java existiert, ist eine grundlegende\
  \ Aufgabe, die das Verifizieren der Pr\xE4senz eines Dateisystemverzeichnisses vor\
  \ dem Lesen, Schreiben oder Ausf\xFChren jeglicher Operationen, die seine Existenz\
  \ erfordern, beinhaltet."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in Java existiert, ist eine grundlegende Aufgabe, die das Verifizieren der Präsenz eines Dateisystemverzeichnisses vor dem Lesen, Schreiben oder Ausführen jeglicher Operationen, die seine Existenz erfordern, beinhaltet. Dies ist entscheidend, um Fehler oder Ausnahmen in Programmen zu vermeiden, die mit dem Dateisystem interagieren, und gewährleistet eine reibungslosere Ausführung und eine bessere Benutzererfahrung.

## Wie:
In Java gibt es mehrere Möglichkeiten, zu überprüfen, ob ein Verzeichnis existiert, hauptsächlich unter Verwendung der Klassen `java.nio.file.Files` und `java.io.File`.

**Verwendung von `java.nio.file.Files`**:

Dies ist der empfohlene Ansatz in neueren Java-Versionen.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Hier den Verzeichnispfad angeben
        String directoryPath = "pfad/zum/verzeichnis";

        // Überprüfung, ob das Verzeichnis existiert
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Das Verzeichnis existiert.");
        } else {
            System.out.println("Das Verzeichnis existiert nicht.");
        }
    }
}
```
**Beispielausgabe**:
```
Das Verzeichnis existiert.
```
Oder
```
Das Verzeichnis existiert nicht.
```

**Verwendung von `java.io.File`**:

Obwohl `java.nio.file.Files` empfohlen wird, kann auch die ältere Klasse `java.io.File` verwendet werden.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Hier den Verzeichnispfad angeben
        String directoryPath = "pfad/zum/verzeichnis";

        // Erstellen eines File-Objekts
        File directory = new File(directoryPath);

        // Überprüfung, ob das Verzeichnis existiert
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Das Verzeichnis existiert.");
        } else {
            System.out.println("Das Verzeichnis existiert nicht.");
        }
    }
}
```
**Beispielausgabe**:
```
Das Verzeichnis existiert.
```
Oder
```
Das Verzeichnis existiert nicht.
```

**Verwendung von Drittanbieter-Bibliotheken**:

Obwohl die Standard-Java-Bibliothek normalerweise für diese Aufgabe ausreicht, bieten Drittanbieter-Bibliotheken wie Apache Commons IO zusätzliche Dateiverarbeitungswerkzeuge an, die in komplexeren Anwendungen nützlich sein könnten.

**Apache Commons IO**:

Fügen Sie zuerst die Apache Commons IO-Abhängigkeit zu Ihrem Projekt hinzu. Danach können Sie deren Funktionen nutzen, um die Existenz eines Verzeichnisses zu überprüfen.

```java
// Annahme, dass Apache Commons IO zum Projekt hinzugefügt ist

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Hier den Verzeichnispfad angeben
        String directoryPath = "pfad/zum/verzeichnis";

        // Verwendung von FileUtils zur Überprüfung
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Das Verzeichnis existiert.");
        } else {
            System.out.println("Das Verzeichnis existiert nicht.");
        }
    }
}
```

**Hinweis**: `FileUtils.directoryContains` prüft, ob ein Verzeichnis eine spezifische Datei enthält, aber indem `null` als zweiter Argument übergeben wird, kann es zur Überprüfung der Existenz des Verzeichnisses verwendet werden. Seien Sie vorsichtig, da dies möglicherweise nicht die geradlinigste oder beabsichtigte Verwendung der Methode ist.

**Beispielausgabe**:
```
Das Verzeichnis existiert.
```
Oder
```
Das Verzeichnis existiert nicht.
```
