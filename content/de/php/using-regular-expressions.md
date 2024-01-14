---
title:                "PHP: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum Regular Expressions verwenden?

Regular Expressions, oder auch reguläre Ausdrücke, sind ein leistungsstarkes Tool für alle Programmierer und Entwickler. Sie ermöglichen es, Textmuster in Strings zu finden und zu manipulieren. Dies spart nicht nur Zeit, sondern auch viele Codezeilen. Wenn du komplexe Such- und Ersetzungsvorgänge in deinem Code durchführen musst, sind Regular Expressions die beste Option.

## Wie man Regular Expressions verwendet

Um Regular Expressions in PHP zu verwenden, musst du die Funktion `preg_match()` verwenden. Diese Funktion nimmt zwei Argumente an: das Muster, nach dem gesucht werden soll, und den String, in dem gesucht werden soll. Zum Beispiel:

```PHP
<?php

$string = "Hallo, mein Name ist Lisa und ich bin 25 Jahre alt.";
$pattern = "/[a-z]+/i";

preg_match($pattern, $string, $matches);

print_r($matches);

// Ausgabe: Array ( [0] => Hallo [1] => mein [2] => Name [3] => ist [4] => Lisa [5] => und [6] => ich [7] => bin [8] => Jahre [9] => alt )
```

In diesem Beispiel wird das Muster `[a-z]+` verwendet, um nach allen Wörtern im String zu suchen, die aus mindestens einem Kleinbuchstaben bestehen. Das `i` am Ende des Musters bedeutet, dass die Groß- und Kleinschreibung ignoriert wird.

Du kannst auch mit regulären Ausdrücken ersetzen, indem du die Funktion `preg_replace()` verwendest. Diese Funktion nimmt drei Argumente an: das Muster, nach dem gesucht werden soll, die Ersetzung und der String, in dem die Ersetzung durchgeführt werden soll. Beispiel:

```PHP
<?php

$string = "Heute ist ein schöner Tag";
$pattern = "/schöner/";
$replace = "herrlicher";

$new_string = preg_replace($pattern, $replace, $string);

echo $new_string;

// Ausgabe: Heute ist ein herrlicher Tag
```

Wie du sehen kannst, wird das Wort "schöner" durch "herrlicher" ersetzt.

## Tiefer Einblick in Regular Expressions

Regular Expressions können noch viel mehr als nur einfache Such- und Ersetzungsvorgänge. Du kannst zum Beispiel mit sogenannten Quantifizierern wie `+` oder `*` arbeiten, um das Muster anzupassen. `+` bedeutet, dass das vorhergehende Zeichen ein oder mehrmals vorkommen muss, während `*` bedeutet, dass es beliebig oft oder gar nicht vorkommen kann. Beispiel:

```PHP
<?php

$string = "aaaab";
$pattern = "/a+b/";
$replace = "c";

$new_string = preg_replace($pattern, $replace, $string);

echo $new_string;

// Ausgabe: cab
```

Hier wird das Muster `a+b` verwendet, was bedeutet, dass mindestens ein "a" vorkommen muss, aber auch mehrere nebeneinander vorkommen können.

Es gibt noch viele weitere Möglichkeiten und Optionen bei der Verwendung von Regular Expressions, die es dir ermöglichen, komplexe Textmanipulationen effizient durchzuführen. Es lohnt sich auf jeden Fall, sich tiefer damit zu beschäftigen.

## Siehe auch

- [PHP Regular Expressions Dokumentation](https://www.php.net/manual/de/book.pcre.php)
- [RegExr - Online Regular Expression Tester](https://regexr.com/)
- [Regular Expressions Cheat Sheet](https://medium.com/factory-mind/regex-tutorial-a-simple-cheatsheet-by-examples-649dc1c3f285) (Englisch)