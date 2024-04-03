---
date: 2024-01-26 03:50:39.876341-07:00
description: "Wie geht das: PHP wird mit einem interaktiven Debugger namens Xdebug\
  \ geliefert. So verwenden Sie ihn. Zuerst stellen Sie sicher, dass Sie Xdebug\u2026"
lastmod: '2024-03-13T22:44:53.976934-06:00'
model: gpt-4-0125-preview
summary: PHP wird mit einem interaktiven Debugger namens Xdebug geliefert.
title: Einsatz eines Debuggers
weight: 35
---

## Wie geht das:
PHP wird mit einem interaktiven Debugger namens Xdebug geliefert. So verwenden Sie ihn.

Zuerst stellen Sie sicher, dass Sie Xdebug installiert und in Ihrer `php.ini`-Datei konfiguriert haben:

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

Schreiben Sie als Nächstes ein einfaches PHP-Skript mit einem Fehler:

```PHP
<?php
function add($a, $b) {
    return $a - $b; // Hoppla! Das sollte ein Plus sein, kein Minus
}

$result = add(1, 2);
echo "Ergebnis ist: $result"; // Ausgabe sollte 3 sein, nicht -1
```

Verwenden Sie eine IDE wie PhpStorm, setzen Sie einen Haltepunkt, indem Sie neben die Zeilennummer klicken. Führen Sie den Debugger aus und beobachten Sie, wie sich Variablen ändern, während Sie die Ausführung durchgehen. Wenn Sie über die Funktion `add` hinweggehen, werden Sie feststellen, dass `$result` -1 wird, was unerwartet ist.

## Tiefergehende Betrachtung:
Historisch gesehen wurde PHP hauptsächlich für kleine Skripte verwendet, und das Debuggen bestand darin, `var_dump()`- und `print_r()`-Anweisungen im Code zu platzieren. Im Laufe der Zeit, mit PHP als einem Schlüsselspieler in der Webentwicklung, kamen ausgefeiltere Werkzeuge wie Xdebug und Zend Debugger zum Einsatz.

Alternativen zu Xdebug sind pcov und phpdbg. Diese bieten verschiedene Funktionen, sind aber möglicherweise nicht so umfangreich wie Xdebug. phpdbg ist ein leichtgewichtiger, spezifisch für PHP entwickelter Debugger, der seit PHP 5.6 mit PHP ausgeliefert wird, und pcov ist ein Treiber für Codeabdeckung.

Wenn Sie einen Debugger implementieren, denken Sie daran, dass Sie den Debugger nie auf Ihrem Produktivserver eingeschaltet lassen sollten, da dies Sicherheitslücken aufdecken und die Leistung verlangsamen kann.

## Siehe auch:
- [Xdebug Dokumentation](https://xdebug.org/docs/)
- [PhpStorm Debugging Guide](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net über phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov auf GitHub](https://github.com/krakjoe/pcov)
