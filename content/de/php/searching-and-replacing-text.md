---
title:                "PHP: Suchen und Ersetzen von Text."
simple_title:         "Suchen und Ersetzen von Text."
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine der grundlegenden Funktionen in der Programmierung. Es ermöglicht uns, schnell und effizient bestimmte Zeichenketten in unseren Codes zu ändern oder zu aktualisieren. In diesem Blog-Beitrag werden wir uns genauer damit beschäftigen, warum dies eine wichtige Fähigkeit beim Programmieren ist und wie man es einfach umsetzen kann.

## Wie man Text in PHP sucht und ersetzt

Um Text in PHP zu suchen und zu ersetzen, verwenden wir die Funktion `str_replace()`. Sie hat drei Parameter: den zu suchenden Text, den Text, mit dem er ersetzt werden soll, und den Text, in dem gesucht werden soll. Wir können auch ein Array als Parameter angeben, um mehrere Suchbegriffe zu ersetzen. Hier ist ein Beispiel-Code:

```PHP
$original_text = "Hallo, Welt!";
$neuer_text = str_replace("Welt", "PHP-Community", $original_text);
echo $neuer_text;
```

Die Ausgabe wird sein:

```
Hallo, PHP-Community!
```

Wir können auch die Funktion `str_ireplace()` verwenden, um die Groß-/Kleinschreibung der Suchbegriffe zu ignorieren. Diese Funktion funktioniert genauso wie `str_replace()`, aber sie ist nicht empfindlich hinsichtlich Groß-/Kleinschreibung. Hier ist ein Beispiel:

```PHP
$original_text = "Hallo, Welt!";
$neuer_text = str_ireplace("welt", "PHP-Community", $original_text);
echo $neuer_text;
```

Die Ausgabe wird ebenfalls sein:

```
Hallo, PHP-Community!
```

## Tiefere Einblicke

Es gibt verschiedene Fälle, in denen wir das Suchen und Ersetzen von Texten nutzen können. Zum Beispiel können wir es verwenden, um Benutzereingaben in einer Form zu überprüfen oder um Platzhalter in Vorlagen zu ersetzen. Wir können auch reguläre Ausdrücke verwenden, um Text mit komplexeren Mustern zu suchen und zu ersetzen. Es gibt viele Möglichkeiten, diese Funktion zu nutzen, und es ist ein grundlegender Teil der effektiven Textverarbeitung in der Programmierung.

## Siehe auch

Hier sind einige hilfreiche Ressourcen, die Sie bei Ihrem Lernprozess unterstützen können:

- [PHP-Dokumentation für str_replace()](https://www.php.net/manual/de/function.str-replace.php)
- [PHP-Dokumentation für str_ireplace()](https://www.php.net/manual/de/function.str-ireplace.php)
- [Reguläre Ausdrücke in PHP](https://www.geeksforgeeks.org/regular-expressions-in-php/)