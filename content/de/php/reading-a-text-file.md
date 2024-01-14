---
title:    "PHP: Einen Textdatei lesen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum 

Textdateien sind ein grundlegender Bestandteil der Programmierung. Sie werden verwendet, um Daten zu speichern, zu organisieren und auszutauschen. Das Lesen von Textdateien kann Ihnen dabei helfen, wichtige Informationen zu extrahieren und auf sie zuzugreifen. In diesem Artikel werden wir uns genauer ansehen, wie man eine Textdatei in PHP lesen kann.

## Wie es geht

Der erste Schritt bei der Lektüre einer Textdatei ist das Öffnen der Datei mit der Funktion `fopen()`. Später wird die Datei geschlossen, nachdem wir sie gelesen haben. Dies ist wichtig, um Ressourcen zu schonen und Speicherlecks zu vermeiden. Ein häufiges Muster beim Lesen einer Textdatei ist das Lesen einer Zeile und dann die Speicherung der Daten in einer Variablen.

```PHP
$myfile = fopen("datei.txt", "r") or die("Datei kann nicht geöffnet werden!");
$zeile = fgets($myfile);
fclose($myfile);
echo $zeile;
```

Der obige Code öffnet eine Datei namens "datei.txt" im Lese-Modus und liest dann die erste Zeile mit der Funktion `fgets()`. Danach wird die Datei mit der Funktion `fclose()` geschlossen und die gelesene Zeile wird auf dem Bildschirm ausgegeben.

Um alle Zeilen einer Datei zu lesen, verwenden wir eine Schleife, die so lange läuft, bis das Ende der Datei erreicht ist.

```PHP
$myfile = fopen("datei.txt", "r") or die("Datei kann nicht geöffnet werden!");
while(!feof($myfile)) {
  $zeile = fgets($myfile);
  echo $zeile . "<br>";
}
fclose($myfile);
```

Dieser Code liest jede Zeile der Datei und gibt sie auf dem Bildschirm aus, bis das Ende der Datei erreicht ist. Mit der `feof()`-Funktion können wir überprüfen, ob das Ende der Datei erreicht wurde.

## Tiefe Eintauchen

Um tief in die Textdatei-Verarbeitung einzutauchen, müssen wir verschiedene Funktionen wie `fread()`, `file()` und viele andere entdecken. Die Verwendung von regulären Ausdrücken kann auch beim Lesen von Textdateien hilfreich sein. Es gibt auch viele erweiterte Funktionen, mit denen Sie Textdateien effizienter bearbeiten können. Es ist wichtig, die Dokumentation zu lesen und zu experimentieren, um ein besseres Verständnis davon zu bekommen, wie Sie Textdateien in Ihren Projekten am besten nutzen können.

---

## Siehe auch

- [PHP-Datei-Handling-Dokumentation](https://www.php.net/manual/de/book.filesystem.php)
- [Reguläre Ausdrücke in PHP](https://www.php.net/manual/de/book.pcre.php)
- [Erweiterte Textdatei-Funktionen](https://www.php.net/manual/de/ref.filesystem.php)