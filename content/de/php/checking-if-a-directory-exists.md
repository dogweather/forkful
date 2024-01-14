---
title:                "PHP: Überprüfen, ob ein Verzeichnis existiert."
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine grundlegende Aufgabe in der PHP-Programmierung. Durch diese Überprüfung können bestimmte Funktionen ausgeführt werden, wie beispielsweise das Erstellen von neuen Dateien oder das Ausführen von Code basierend auf dem Vorhandensein eines bestimmten Verzeichnisses. Es ist ein wichtiger Teil der Fehlerbehandlung und ermöglicht es Programmen, sich an die Umgebung anzupassen und entsprechend zu reagieren.

## Wie geht es

Es gibt mehrere Methoden, um in PHP zu überprüfen, ob ein Verzeichnis existiert. Eine Möglichkeit ist die Verwendung der "file_exists()" -Funktion, die überprüft, ob eine angegebene Datei oder ein Verzeichnis vorhanden ist. Hier ist ein Beispielcode, der zeigt, wie dies gemacht werden kann:

```PHP
<?php 
if(file_exists("/home/username/Documents")){
  echo "Verzeichnis existiert.";
} else{
  echo "Verzeichnis nicht gefunden.";
}
?>
```

Dieser Code überprüft, ob das Verzeichnis "Documents" im Benutzerordner vorhanden ist und gibt dementsprechend eine Meldung aus. Eine andere Option ist die Verwendung der "is_dir()" -Funktion, die speziell überprüft, ob es sich bei dem angegebenen Pfad um ein Verzeichnis handelt. Hier ist ein Beispielcode:

```PHP
<?php 
if(is_dir("/var/www/html")){
  echo "Ja, es ist ein Verzeichnis.";
} else{
  echo "Nein, es handelt sich nicht um ein Verzeichnis.";
}
?>
```

Die "is_dir()" -Funktion gibt entweder true oder false zurück, abhängig davon, ob das angegebene Speicherort ein Verzeichnis ist. Weitere Informationen zu diesen Funktionen und anderen Methoden zur Überprüfung des Vorhandenseins von Verzeichnissen finden Sie in der Dokumentation von PHP.

## Tiefer eintauchen

Um weiter in die Überprüfung von Verzeichnissen in PHP einzusteigen, ist es wichtig zu verstehen, wie das Dateisystem funktioniert. PHP bietet mehrere Funktionen zum Lesen und Bearbeiten von Dateien und Ordnern, einschließlich der obigen "file_exists()" und "is_dir()" -Funktionen. Diese Methoden können auch in Kombination mit anderen Dateisystemfunktionen verwendet werden, um komplexe Abläufe auszuführen, wie beispielsweise das Durchsuchen von Ordnern oder das Ändern von Dateinamen.

Die Überwachung von Verzeichnissen kann auch hilfreich sein, um mögliche Sicherheitslücken zu erkennen. Durch die Überprüfung und Aktualisierung von Berechtigungen für bestimmte Verzeichnisse können Entwickler sicherstellen, dass unerwünschte Zugriffe oder Veränderungen an Dateien verhindert werden.

## Siehe auch

- [PHP Filesystem Developer Guide](https://www.php.net/manual/es/book.filesystem.php)
- [Using files and directories in PHP](https://www.php.net/manual/es/book.filesystem.php)
- [5 Ways to Check if a Directory Exists in PHP](https://www.shellhacks.com/php-check-if-directory-exists/)
- [PHP 7 - Dateisystem](https://www.tutorialspoint.com/php7/php7_filesystem.htm)