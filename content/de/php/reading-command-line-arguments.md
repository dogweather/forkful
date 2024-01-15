---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "PHP: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du PHP-Entwickler bist, hast du wahrscheinlich schon einmal von "Command Line Arguments" gehört. Diese Begriffe beziehen sich auf die Argumente, die ein Skript erhält, wenn es über die Befehlszeile aufgerufen wird. Das Lesen von Befehlszeilenargumenten kann sehr nützlich sein, wenn du Skripte schreibst, die von der Befehlszeile aus gestartet werden sollen. In diesem Artikel werde ich dir zeigen, wie du Befehlszeilenargumente in PHP lesen kannst und warum es wichtig ist, dies zu wissen.

## Wie geht das?

Um Befehlszeilenargumente in PHP zu lesen, können wir die Funktion "getopt()" verwenden. Sie erwartet zwei Argumente: den Befehl (also den Namen deines PHP-Skripts) und eine Zeichenkette mit den erwarteten Argumenten. Schauen wir uns das an einem Beispiel an:

```PHP
<?php
// Aufruf des Skripts: php beispiel.php -u Benutzer -p Passwort -d Datenbank
$options = getopt("u:p:d:");

if(isset($options["u"])) {
    echo "Benutzer: " . $options["u"] . PHP_EOL;
}

if(isset($options["p"])) {
    echo "Passwort: " . $options["p"] . PHP_EOL;
}

if(isset($options["d"])) {
    echo "Datenbank: " . $options["d"] . PHP_EOL;
}
?>
```

In diesem Beispiel lesen wir drei Befehlszeilenargumente: "-u" für den Benutzernamen, "-p" für das Passwort und "-d" für den Datenbanknamen. Diese Argumente werden dann in ein Array gespeichert, das wir mit der Funktion "isset()" überprüfen können. Wenn das entsprechende Argument gesetzt ist, wird der Wert ausgegeben.

Wenn wir also unser Skript mit den obigen Argumenten aufrufen, erhalten wir folgende Ausgabe:

```
Benutzer: Benutzer
Passwort: Passwort
Datenbank: Datenbank
```

## Tiefer tauchen

Jetzt wo du weißt, wie du Befehlszeilenargumente in PHP lesen kannst, lass uns etwas tiefer tauchen und uns einige nützliche Funktionen anschauen, die uns dabei helfen können.

Zunächst einmal können wir über die Funktion "getopt()" auch optionale Argumente definieren, indem wir ein ":" oder ein "=" hinter den Buchstaben setzen. Zum Beispiel könnten wir in unserem Beispiel sagen, dass das Argument "-p" ein erforderliches Argument ist, während das Argument "-d" optional ist. Dafür müssen wir die Zeichenkette in unserer "getopt()" Funktion wie folgt anpassen: "u:p:d::".

Außerdem können wir auch überprüfen, ob ein spezielles Argument gesetzt wurde oder nicht und entsprechend darauf reagieren. Zum Beispiel könnten wir überprüfen, ob die Option "-v" (für "verbose") gesetzt wurde und dann zusätzliche Informationen während der Ausführung unseres Skripts ausgeben.

Zuletzt ist es auch möglich, den Befehl dynamisch zu übergeben. Anstatt die Befehlszeile statisch in unserem Skript zu definieren, können wir sie auch in einer Variablen speichern und dann an "getopt()" übergeben. Dadurch wird unser Skript noch flexibler und kann mit verschiedenen Befehlen aufgerufen werden.

## Siehe auch

- [PHP Dokumentation zu getopt()](https://www.php.net/manual/de/function.getopt.php)
- [Weitere Informationen zu Befehlszeilenargumenten in PHP](https://www.sitepoint.com/command-line-php-argparse)