---
title:    "PHP: String in Kleinbuchstaben umwandeln"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Man könnte sich fragen, warum man überhaupt den Aufwand betreiben sollte, eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Die Antwort ist einfach: In bestimmten Situationen kann es wichtig sein, dass die Groß- und Kleinschreibung von Wörtern einheitlich ist, um beispielsweise Suchanfragen oder Vergleiche korrekt ausführen zu können.

## Wie man eine Zeichenfolge in Kleinbuchstaben umwandelt

Es gibt mehrere Möglichkeiten, eine Zeichenfolge in Kleinbuchstaben umzuwandeln. Eine davon ist die Verwendung der PHP-Funktion `strtolower()`. Diese Funktion nimmt als Parameter die zu konvertierende Zeichenfolge und gibt sie in Kleinbuchstaben zurück.

```PHP
$word = "HELLO";
echo strtolower($word); // Output: hello
```

Eine andere Möglichkeit ist die Verwendung der PHP-Funktion `mb_strtolower()`. Diese Funktion ist speziell für Multibyte-Zeichen wie z.B. Umlaute geeignet.

```PHP
$word = "ÜBER";
echo mb_strtolower($word, 'UTF-8'); // Output: über
```

## Tiefergehende Informationen

Bei der Verwendung der `strtolower()` Funktion ist es wichtig zu beachten, dass die Funktion auf die aktuelle Systemsprache des Servers begrenzt ist. Das bedeutet, dass die Funktion unter Umständen auch Zeichenfolgen in andere Sprachen als Englisch nicht korrekt umwandeln kann. In solchen Fällen sollte die `mb_strtolower()` Funktion verwendet werden.

Außerdem ist es wichtig zu beachten, dass bei beiden Funktionen die Rückgabe der umgewandelten Zeichenfolge ein neues String-Objekt ist und nicht das ursprüngliche. Das bedeutet, dass der ursprüngliche String unverändert bleibt.

## Siehe auch

- [PHP strtolower() Funktion](https://www.php.net/manual/de/function.strtolower.php)
- [PHP mb_strtolower() Funktion](https://www.php.net/manual/de/function.mb-strtolower.php)