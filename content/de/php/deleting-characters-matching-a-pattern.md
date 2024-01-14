---
title:                "PHP: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

Warum: Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Fähigkeit beim Programmieren. Es kann dabei helfen, Daten zu bereinigen oder unerwünschte Zeichen aus einer Zeichenkette zu entfernen.

Wie: Die grundlegende Methode zum Löschen von Zeichen in PHP ist die Verwendung der Funktion `preg_replace()`. Diese Funktion erlaubt es uns, ein Muster anzugeben, nach dem gesucht werden soll, und dann entweder eine leere Zeichenkette oder einen Ersatzwert anzugeben, durch den die übereinstimmenden Zeichen ersetzt werden sollen.

```PHP
$sample_string = "Dies ist ein Beispieltext123, der einige unerwünschte Zeichen123 enthält.";

// Löscht alle Zahlen aus der Zeichenkette
$result = preg_replace("/[0-9]+/", "", $sample_string);

echo $result; // Output: Dies ist ein Beispieltext, der einige unerwünschte Zeichen enthält.
```

Wir können auch Platzhalter wie `.` verwenden, um alle Zeichen an einer bestimmten Stelle zu löschen. Zum Beispiel, wenn wir alle Zeichen nach dem 10. Zeichen löschen möchten, können wir das folgende Muster verwenden:

```PHP
$sample_string = "Dies ist ein Beispieltext, der einige unerwünschte Zeichen enthält.";

// Löscht alles ab dem 11. Zeichen
$result = preg_replace("/.{11,}/", "", $sample_string);

echo $result; // Output: Dies ist ein
```

Tiefer Einblick: Es gibt eine Vielzahl von Möglichkeiten, Zeichen in PHP zu löschen, je nachdem, welche Art von Muster wir suchen. Wir können reguläre Ausdrücke verwenden, um sehr spezifische Zeichenfolgen zu suchen und zu löschen, oder wir können auch eingebaute Funktionen wie `str_replace()` verwenden, um bestimmte Zeichen oder Zeichenfolgen zu ersetzen. Es ist wichtig zu beachten, dass die Wahl der richtigen Methode davon abhängen kann, wie komplex das zu löschende Muster ist und welche Art von Daten wir bearbeiten.

Siehe auch: Weitere Informationen und Beispiele zur Verwendung von regulären Ausdrücken in PHP finden Sie in der offiziellen PHP-Dokumentation (http://php.net/manual/de/book.pcre.php). Hier sind einige nützliche Ressourcen für die Verwendung von regulären Ausdrücken in PHP:

- https://www.w3schools.com/php/php_regex.asp
- https://www.php.net/manual/en/function.preg-replace.php
- https://www.regular-expressions.info/rlanguage.html