---
title:                "PHP: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man beim Programmieren auf PHP substrings (Teilstrings) anpassen möchte. Ein häufiger Grund ist zum Beispiel, um bestimmte Teile einer textbasierten Eingabe zu extrahieren. Oder auch um eine URL in einzelne Komponenten zu zerlegen. Egal aus welchem Grund, die Extraktion von Substrings ist eine wichtige Fähigkeit in der PHP-Programmierung.

## Wie geht man vor?

Um einen Substring in PHP zu extrahieren, gibt es verschiedene Methoden. Eine davon ist die Verwendung der Funktion `substr()`. Hier ist ein Beispiel:

```PHP
$text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";

// Extrahiere die ersten 5 Zeichen
echo substr($text, 0, 5);

// Ausgabe: Lorem
```

Mit dieser Funktion können wir den gewünschten Teilstring auswählen, indem wir den Startindex und die Länge des Substrings angeben. In diesem Fall haben wir den Index 0 ausgewählt, was bedeutet, dass wir bei dem ersten Zeichen im String beginnen. Die Länge des Substrings beträgt 5 Zeichen.

Hier ist ein weiteres Beispiel, in dem wir die Funktion `substr()` verwenden, um einen Teilstring aus einer URL zu extrahieren:

```PHP
$url = "https://www.example.com/blog/post/123";

// Extrahiere den Teilstring "blog/post"
echo substr($url, 14, 9);

// Ausgabe: blog/post
```

In diesem Beispiel haben wir den Teilstring "blog/post" aus einer URL extrahiert, indem wir den Startindex bei 14 gesetzt haben (nach "https://www.example.com/") und eine Länge von 9 Zeichen angegeben haben.

## Tiefeneintauchen

Wenn es darum geht, Substrings in PHP zu extrahieren, gibt es noch viele weitere Funktionen und Methoden, die man verwenden kann. Eine davon ist die Funktion `strpos()`, mit der man den Index des ersten Vorkommens eines bestimmten Teilstrings in einem größeren String finden kann. Hier ist ein Beispiel:

```PHP
$text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";

// Finde den Index des ersten Vorkommens von "dolor"
echo strpos($text, "dolor");

// Ausgabe: 12
```

In diesem Beispiel haben wir den Index 12 ausgegeben, was dem Anfang des Teilstrings "dolor" entspricht. Damit können wir zum Beispiel den Teilstring "sit amet" extrahieren, indem wir `substr()` und `strpos()` kombinieren.

## Siehe auch

- [PHP substr() Funktion](https://www.php.net/manual/de/function.substr.php)
- [PHP strpos() Funktion](https://www.php.net/manual/de/function.strpos.php)
- [PHP Teilstrings extrahieren](https://www.php-einfach.de/php-tutorial/php-substring/)
- [PHP String-Funktionen](https://www.php-einfach.de/php-tutorial/php-string-funktionen/)