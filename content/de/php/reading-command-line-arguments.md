---
title:                "PHP: Lesen von Befehlszeilenargumenten"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man sich mit dem Lesen von Befehlszeilenargumenten in der PHP-Programmierung beschäftigen sollte. Einer davon ist, dass es eine effiziente Möglichkeit ist, externe Eingaben in ein Skript zu integrieren. Außerdem kann es bei der Entwicklung von Kommandozeilenanwendungen hilfreich sein.

## Wie geht das
Das Lesen von Befehlszeilenargumenten in PHP ist relativ einfach. Zunächst müssen wir die `$_SERVER['argv']`-Variable abfragen, um an die Argumente zu gelangen. Diese Variable enthält einen Array mit allen übergebenen Argumenten. Hier ist ein Beispielcode, der die Argumente ausgibt:

```PHP
<?php
for ($i = 0; $i < count($_SERVER['argv']); $i++) {
    echo "Argument #" . $i . ": " . $_SERVER['argv'][$i] . "\n";
}
```

Wenn wir dieses Skript mit dem Befehl `php script.php argument1 argument2` ausführen, wird die Ausgabe wie folgt aussehen:

```
Argument #0: script.php
Argument #1: argument1
Argument #2: argument2
```

## Tiefer gehende Information
Es gibt verschiedene Methoden, um Befehlszeilenargumente zu verarbeiten. In unserem Beispiel haben wir einfach durch die `$_SERVER['argv']`-Variable iteriert, aber es gibt auch Funktionen wie `getopt()`, die die Argumente in einer strukturierteren Form zurückgeben.

Außerdem ist es wichtig zu beachten, dass Befehlszeilenargumente in der Regel als Strings behandelt werden. Wenn Sie also Zahlen oder andere Datenformate benötigen, müssen Sie diese entsprechend konvertieren.

## Siehe auch
- [PHP-Dokumentation zu Lesen von Befehlszeilenargumenten](https://www.php.net/manual/en/features.commandline.php)
- [Blog-Beitrag: 10 Tipps für die Arbeit mit Befehlszeilenargumenten in PHP](https://www.example.com/10-tipps-befehlszeilenargumente-php)
- [Video-Tutorial: Befehlszeilenargumente in PHP lesen und verarbeiten](https://www.example.com/befehlszeilenargumente-php-tutorial)