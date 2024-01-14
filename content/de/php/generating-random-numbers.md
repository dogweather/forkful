---
title:    "PHP: Erzeugung zufälliger Zahlen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung ist es oft notwendig, zufällige Zahlen zu generieren. Dies kann beispielsweise für die Verwendung in Spielen, bei der Erstellung von Passwörtern oder für statistische Zwecke erforderlich sein. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie wir mit PHP zufällige Zahlen generieren können.

## Wie man es macht

Das Generieren von zufälligen Zahlen in PHP ist relativ einfach. Wir können die Funktion `rand()` verwenden, um eine zufällige Ganzzahl zwischen einem angegebenen Minimum und Maximum zu erzeugen. Schauen wir uns ein Beispiel an:

```PHP
$random_number = rand(1, 10);
echo "Die zufällige Zahl ist: " . $random_number;
```

Dieser Code wird eine zufällige Zahl zwischen 1 und 10 generieren und sie dann mit der `echo` Anweisung ausgeben.

Wir können auch eine zufällige Gleitkommazahl mithilfe der Funktion `mt_rand()` erzeugen. Diese Funktion verwendet einen stärkeren Zufallszahlengenerator und ist daher für sicherheitsrelevante Anwendungen geeignet. Hier ist ein Beispiel:

```PHP
$random_float = mt_rand(0, 1);
echo "Die zufällige Gleitkommazahl ist: " . $random_float;
```

Wie Sie sehen, wird die Funktion `mt_rand()` nur zwei Parameter akzeptieren, da der erste Parameter als das tatsächliche Minimum und der zweite Parameter als das tatsächliche Maximum angesehen wird.

## Tiefer eintauchen

Wenn wir uns tiefer mit dem Generieren von zufälligen Zahlen in PHP beschäftigen, werden wir feststellen, dass die `rand()` und `mt_rand()` Funktionen eine allgemeine Schwäche haben. Sie können eine relativ vorhersehbare Folge von Zahlen erzeugen, wenn sie nicht richtig initialisiert werden. Dies kann jedoch gelöst werden, indem man zuerst die `srand()` Funktion verwendet, um den Zufallsgenerator zu initialisieren. Hier ist ein Beispiel, wie wir dies tun können:

```PHP
srand(42);
$random_number = rand(1, 10);
echo "Die zufällige Zahl ist: " . $random_number;
```

Durch die Verwendung von `srand()` mit einem spezifischen Startwert, in diesem Fall 42, wird sichergestellt, dass wir jedes Mal, wenn das Skript ausgeführt wird, die gleiche zufällige Zahl erhalten.

Es gibt auch zusätzliche Funktionen in PHP, wie `random_int()` und `random_bytes()`, die für die sichere Generierung von Zufallszahlen empfohlen werden. Sie können diese Funktionen verwenden, wenn Sie Sicherheitsbedenken haben oder Zufallszahlen für kryptografische Zwecke benötigen.

## Siehe auch

Diese Artikel könnten für Sie nützlich sein, um weiterlesen zu können:

- [PHP's offizielle Dokumentation zu Zufallszahlen](https://www.php.net/manual/de/function.rand.php)
- [Wikipedia Artikel über Zufallszahlen](https://de.wikipedia.org/wiki/Zufallszahl)
- [PHP Tutorial: Random Number Generator](https://www.w3schools.com/php/func_math_rand.asp)