---
title:    "PHP: Suchen und Ersetzen von Text"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Warum

Das Suchen und Ersetzen von Text ist eine einfache und effiziente Methode, um Text innerhalb eines Strings oder einer Datei zu bearbeiten. Es kann besonders hilfreich sein, wenn man zum Beispiel eine große Anzahl von Dateien durchsuchen und bestimmte Wörter oder Zeichen ersetzen möchte.

## Wie gehts

Um Text in PHP zu suchen und zu ersetzen, verwendet man die Funktion `str_replace()`. Diese Funktion akzeptiert drei Parameter: den Text, der gesucht werden soll, den Text, durch den er ersetzt werden soll, und die Variable, in der der Text gespeichert ist. Zum Beispiel:

```PHP
$text = "Das ist ein Beispielsatz.";
$new_text = str_replace("Beispiel", "neues Beispiel", $text);
```

Dieses Beispiel sucht nach dem Wort "Beispiel" und ersetzt es durch "neues Beispiel". Das Ergebnis ist `Das ist ein neues Beispielsatz.` Die Funktion `str_replace()` kann auch verwendet werden, um mehrere Wörter oder Zeichen auf einmal zu ersetzen, indem man Arrays als Parameter angibt.

## Tiefer tauchen

Es gibt viele nützliche Funktionen in PHP, um Text zu suchen und zu ersetzen. Zum Beispiel gibt es die Funktion `preg_replace()`, die es ermöglicht, mit regulären Ausdrücken zu arbeiten. Reguläre Ausdrücke sind Muster, die verwendet werden können, um Text zu suchen und zu ersetzen, basierend auf bestimmten Kriterien. Dies kann besonders hilfreich sein, wenn man sehr spezifische Such- und Ersetzungsaufgaben hat.

Zusätzlich gibt es auch die `strtr()` Funktion, die es ermöglicht, mehrere Zeichen oder Wörter gleichzeitig zu ersetzen. Dies kann bei der Bearbeitung von Dateien mit hoher Anzahl von Wiederholungen sehr hilfreich sein.

# Siehe auch

- Offizielle PHP-Dokumentation zu `str_replace()`: https://www.php.net/manual/de/function.str-replace.php
- Reguläre Ausdrücke in PHP: https://www.php.net/manual/de/function.preg-replace.php
- `strtr()` Funktion in PHP: https://www.php.net/manual/de/function.strtr.php