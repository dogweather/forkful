---
title:    "PHP: Die Länge eines Strings bestimmen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Das Auffinden der Länge einer Zeichenkette ist ein grundlegender Schritt für jeden PHP Programmierer. Es ermöglicht die Manipulation von Texten und die Überprüfung von Eingaben, was für viele Anwendungen unerlässlich ist.

# Wie man es macht

Um die Länge einer Zeichenkette in PHP zu finden, verwenden wir die Funktion `strlen()`. Sie gibt die Anzahl der Zeichen in einer Zeichenkette zurück. Schauen wir uns ein Beispiel an:

```PHP
$wunsch = "Ich möchte ein Programmierer werden!";
echo strlen($wunsch);
```

Die Ausgabe dieses Codes wäre "33", da es 33 Zeichen in der Zeichenkette "Ich möchte ein Programmierer werden!" gibt. Beachten Sie, dass auch Leerzeichen als Zeichen gezählt werden.

Eine weitere nützliche Funktion ist `mb_strlen()`, die speziell für multibyte Zeichen wie z.B. Umlaute in der deutschen Sprache ausgelegt ist.

# Tiefentauchen

Es ist wichtig zu beachten, dass die Länge einer Zeichenkette in PHP nicht immer dem entspricht, was wir auf den ersten Blick erwarten würden. Zum Beispiel zählt `strlen()` pro Unicode-Zeichen und nicht pro Buchstabe, daher kann es zu unerwarteten Ergebnissen kommen, wenn wir mit Zeichen außerhalb des ASCII-Zeichensatzes arbeiten.

Außerdem kann die Verwendung von `strlen()` auf UTF-8-kodierte Zeichenketten zu falschen Ergebnissen führen, da die Funktion nicht auf die mögliche Größe eines einzelnen Zeichens in diesem Zeichensatz ausgelegt ist. In solchen Fällen ist es ratsam, `mb_strlen()` zu verwenden oder die Zeichenkette in eine andere Kodierung zu konvertieren.

# Siehe auch
- [PHP-Dokumentation zu strlen()](https://www.php.net/manual/de/function.strlen.php)
- [PHP-Dokumentation zu mb_strlen()](https://www.php.net/manual/de/function.mb-strlen.php)
- [Artikel über den Umgang mit UTF-8 in PHP](https://www.php.net/manual/de/reference.pcre.pattern.modifiers.php#32117)