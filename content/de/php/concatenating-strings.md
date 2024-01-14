---
title:    "PHP: Zeichenketten verketten"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum

Concatenating oder Verketten von Strings ist eine essentielle Fähigkeit in der PHP Programmierung. Durch das Verbinden mehrerer Textbausteine können dynamische Inhalte erstellt werden, die für den Nutzer personalisiert und ansprechender sind. Zudem wird der Code übersichtlicher und leichter zu lesen.

## Wie geht das?

In PHP gibt es mehrere Möglichkeiten, Strings zu verketten. Eine davon ist die Verwendung des Punktes (".") als Verkettungsoperator. Schauen wir uns das anhand eines einfachen Beispiels an:

```PHP
$name = "Peter";
$age = 26;
$message = "Ich heiße " . $name . " und ich bin " . $age. " Jahre alt.";
echo $message;
```

Die Ausgabe dieses Codes wäre: "Ich heiße Peter und ich bin 26 Jahre alt." Hier wurden die Variablen $name und $age mithilfe des Punktes in den String eingefügt.

Es ist auch möglich, Verkettungen innerhalb von Strings mit geschweiften Klammern und dem Dollarzeichen zu realisieren. Das sieht dann so aus:

```PHP
$name = "Peter";
$message = "Ich heiße {$name} und ich bin {$age} Jahre alt.";
echo $message;
```

Das Ergebnis wäre dasselbe wie beim vorherigen Beispiel.

## Deep Dive

Bei der Verkettung von Strings ist es wichtig zu beachten, dass der Ergebnis-String die richtige Syntax aufweist. So müssen beispielsweise Leerzeichen und Zeilenumbrüche manuell hinzugefügt werden. Auch sollte darauf geachtet werden, dass die entsprechenden Variablen vor der Verwendung auch tatsächlich initialisiert wurden.

Es ist auch möglich, mehr als zwei Strings miteinander zu verketten. Dazu können einfach weitere Punkte und Variablen hinzugefügt werden.

## Siehe auch

- [PHP String Verkettung](https://www.php.net/manual/de/language.operators.string.php)
- [PHP String Funktionen](https://www.php.net/manual/de/ref.strings.php)
- [W3Schools - PHP String Verkettung](https://www.w3schools.com/php/php_operators.asp)

Hoffentlich konnte dieser kleine Überblick dir helfen, besser zu verstehen, wie du Strings in PHP miteinander verkettet. Viel Spaß beim Programmieren!