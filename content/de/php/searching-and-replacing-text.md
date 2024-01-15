---
title:                "Suchen und Ersetzen von Text."
html_title:           "PHP: Suchen und Ersetzen von Text."
simple_title:         "Suchen und Ersetzen von Text."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Suchen und Ersetzen von Text ist ein häufiger Bestandteil der Programmierung. Es ermöglicht es uns, effizient große Mengen von Text zu bearbeiten und Fehler in unserem Code zu korrigieren. In diesem Artikel werden wir uns ansehen, wie man in PHP Text suchen und ersetzen kann und warum dies eine nützliche Fähigkeit ist.

## Wie es geht

Um Text in PHP zu suchen und zu ersetzen, verwenden wir die Funktion `str_replace()`. Sie erwartet drei Parameter: den Text, nach dem wir suchen, den Text, durch den wir ihn ersetzen möchten, und den Text, in dem wir suchen möchten. Die grundlegende Syntax sieht folgendermaßen aus:

```PHP
str_replace($search, $replace, $text);
```

Schauen wir uns ein Beispiel an, um dies besser zu verstehen. Angenommen, wir haben einen String namens `$text` mit dem Inhalt "Guten Morgen!". Wenn wir nun "Morgen" durch "Tag" ersetzen möchten, können wir dies folgendermaßen tun:

```PHP
echo str_replace("Morgen", "Tag", $text);
```

Die Ausgabe wäre dann "Guten Tag!". Wir können auch Arrays verwenden, um mehrere Suchbegriffe gleichzeitig zu ersetzen. In diesem Fall wird jedes Element des Arrays als Suche und Ersatzpaar verwendet. Schauen wir uns ein Beispiel an:

```PHP
$search = array("Morgen", "Guten");
$replace = array("Abend", "Hallo");
echo str_replace($search, $replace, $text);
```

Die Ausgabe wäre "Hallo Abend!". Beachten Sie, dass die Reihenfolge der Elemente in den Arrays wichtig ist, da sie den Platzhaltern `search` und `replace` in der `str_replace()` Funktion entsprechen.

## Tiefergehende Informationen

Neben dem `str_replace()` gibt es in PHP auch die Funktion `preg_replace()`, die die Verwendung von regulären Ausdrücken für das Suchen und Ersetzen von Text ermöglicht. Dies ist besonders nützlich, wenn das zu suchende Muster etwas komplexer ist oder wir nur bestimmte Bereiche eines Textes ersetzen möchten. Ein regulärer Ausdruck besteht aus einem Muster und optionalen Modifikatoren, die die Suche anpassen können. Wir können dann auch die Funktion `preg_match()` verwenden, um zu überprüfen, ob ein bestimmtes Muster in einem String enthalten ist.

## Siehe auch

- [PHP Offizielle Dokumentation über str_replace()](https://www.php.net/manual/de/function.str-replace.php)
- [PHP Offizielle Dokumentation über preg_replace()](https://www.php.net/manual/de/function.preg-replace.php)
- [Tutorial für reguläre Ausdrücke in PHP](https://www.regular-expressions.info/php.html)