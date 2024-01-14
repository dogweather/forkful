---
title:                "PHP: Debug-Ausgabe drucken"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals versucht haben, ein PHP-Programm zu debuggen, wissen Sie wahrscheinlich, wie zeitaufwändig und frustrierend es sein kann, Fehler aufzuspüren. Das Printen von Debug-Ausgaben kann jedoch ein nützliches Werkzeug sein, um diesen Prozess zu erleichtern und schneller zum gewünschten Ergebnis zu gelangen.

## Wie man Debug-Ausgaben in PHP verwendet

Um Debug-Ausgaben in PHP zu verwenden, gibt es eine einfache Funktion namens "echo", die Sie in Ihr Code einfügen können, um bestimmte Werte oder Variablen zu drucken. Hier ist ein Beispiel:

```PHP
$name = "Max Mustermann";
echo $name;
```

Die Ausgabe wird dann "Max Mustermann" sein. Sie können auch mehrere Variablen durch Kommas trennen, um sie zusammen auszugeben.

```PHP
$name = "Max";
$age = 30;
echo $name, " ist ", $age, " Jahre alt.";
```

Die Ausgabe wird dann "Max ist 30 Jahre alt." sein.

Es ist auch hilfreich, bestimmte Werte oder Variablen innerhalb einer Schleife zu printen, um zu überprüfen, ob der Code wie erwartet funktioniert.

```PHP
for ($i = 0; $i < 5; $i++) {
  echo $i;
}
```

Dies wird die Zahlen von 0 bis 4 ausgeben.

## Tiefergehende Informationen zu Debug-Ausgaben

Es gibt verschiedene Arten von Debug-Ausgaben, die Sie nutzen können, um verschiedene Aspekte Ihres Codes zu überprüfen. Zum Beispiel können Sie die Funktion "print_r" verwenden, um komplexe Variablen oder Arrays auszugeben, was besonders nützlich ist, wenn Sie deren Struktur überprüfen möchten.

```PHP
$fruits = array("Apfel", "Banane", "Orange");
print_r($fruits);
```

Dies wird die folgende Ausgabe erzeugen:

```
Array
(
  [0] => Apfel
  [1] => Banane
  [2] => Orange
)
```

Eine weitere nützliche Funktion ist "var_dump", die noch detailliertere Informationen liefert und auch den Datentyp der Variablen ausgibt.

```PHP
$number = 25;
var_dump($number);
```

Die Ausgabe wird dann "int(25)" sein.

Es gibt auch spezielle Debugging-Tools wie Xdebug, die eine umfassende und visuelle Debugging-Umgebung bieten. Diese Tools erfordern jedoch eine zusätzliche Konfiguration und sind für Anfänger möglicherweise etwas komplex.

## Siehe auch

- [PHP-Dokumentation: Print-Funktionen](https://www.php.net/manual/de/language.basic-syntax.printing.php)
- [Xdebug: Offizielle Website](https://xdebug.org/)
- [Debugging-Tutorials auf YouTube](https://www.youtube.com/playlist?list=PLfdtiltiRHWHjTPiFDRdTOPtSyYfz3iLW)