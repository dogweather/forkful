---
title:    "PHP: Zufallszahlen generieren"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Warum

In der Programmierung gibt es oft die Notwendigkeit, zufällige Zahlen zu generieren. Dies kann zum Beispiel für die Erstellung von Passwörtern, die Durchführung von Simulationen oder die Optimierung von Algorithmen erforderlich sein.

# Wie man zufällige Zahlen in PHP generiert

Um zufällige Zahlen in PHP zu generieren, gibt es verschiedene Funktionen, die verwendet werden können. Eine der einfachsten ist die `rand()`-Funktion, die zwei Parameter erwartet: den Start- und den Endwert des gewünschten Zahlenbereichs. Die folgenden Zahlen werden mit `rand(1, 10)` generiert:

```PHP
echo rand(1, 10); // Output: 3
echo rand(1, 10); // Output: 7
echo rand(1, 10); // Output: 10
```

Eine weitere Möglichkeit ist die `mt_rand()`-Funktion, die als Parameter auch den Start- und Endwert erwartet. Die Funktionsweise ist ähnlich wie bei `rand()`, jedoch wird ein Algorithmus verwendet, der eine höhere Zufälligkeit gewährleistet. Die folgenden Zahlen werden mit `mt_rand(1, 10)` generiert:

```PHP
echo mt_rand(1, 10); // Output: 4
echo mt_rand(1, 10); // Output: 9
echo mt_rand(1, 10); // Output: 2
```

Zudem gibt es noch die Möglichkeit, eine zufällige Zahl aus einem Array von Werten zu wählen, indem man die `array_rand()`-Funktion verwendet. Diese Funktion akzeptiert als Parameter das Array, aus dem eine zufällige Zahl ausgewählt werden soll. Die folgenden Zahlen werden aus dem Array `[1,2,3,4,5,6,7,8,9,10]` ausgewählt:

```PHP
$array = [1,2,3,4,5,6,7,8,9,10];
echo $array[array_rand($array)]; // Output: 5
echo $array[array_rand($array)]; // Output: 9
echo $array[array_rand($array)]; // Output: 2
```

Es ist auch möglich, zufällige Zeichenfolgen zu generieren, indem man die `str_shuffle()`-Funktion verwendet. Diese Funktion akzeptiert als Parameter die Zeichenfolge, die gemischt werden soll. Die folgenden Zeichenfolgen werden gemischt:

```PHP
echo str_shuffle("abcdefghijklmnopqrstuvwxyz"); // Output: bmkpqchjfvyxtoqznsdlwaruegi
echo str_shuffle("1234567890"); // Output: 8457286039
```

# Tiefere Einblicke in die Generierung von zufälligen Zahlen

Die Verwendung von `rand()` oder `mt_rand()` kann zu Ergebnissen führen, die nicht ganz zufällig sind, da sie auf einem Algorithmus basieren. Eine bessere Möglichkeit ist es, die `random_int()`-Funktion zu verwenden, die ab PHP 7 zur Verfügung steht. Diese Funktion verwendet eine kryptografisch sichere Methode, um zufällige Zahlen zu generieren. Sie erwartet ebenfalls zwei Parameter für den Start- und Endwert. Die folgenden Zahlen werden mit `random_int(1, 10)` generiert:

```PHP
echo random_int(1, 10); // Output: 8
echo random_int(1, 10); // Output: 3
echo random_int(1, 10); // Output: 6
```

Zudem ist es wichtig zu beachten, dass die Generierung von Zufallszahlen in PHP auch von der Konfiguration des Servers und der verwendeten Umgebung abhängig sein kann. Es ist daher ratsam, sich mit den Dokumentationen und Empfehlungen von PHP auseinanderzusetzen, um eine sichere und zuverlässige Generierung von Zufallszahlen zu gewährleisten.

# Siehe auch

- [PHP-Dokumentation zur Generierung von Zufallszahlen](https://www.php.net/manual/en/function.rand.php)
- [Randomness in PHP by Alex Mills](https://medium.com/@alexmillard/randomness-in-php-66b831ba6b1c)
- [Generating random numbers in PHP by Petar Nikolow](https://petarnikolo.dev/php-tutorials/generating-random-numbers-in-php)