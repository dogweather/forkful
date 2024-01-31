---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T03:50:39.876341-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger ist ein Werkzeug, das Programmierern hilft zu verstehen, was ihr Code tatsächlich macht, während er ausgeführt wird. Es ist die Lupe, die uns erlaubt, uns auf Fehler zu fokussieren – diese nervigen Probleme, die unsere Programme abstürzen lassen oder falsche Antworten ausspucken – und sie zu beseitigen. Wir verwenden Debugger, weil sie uns Stunden von Print-Befehlen und Ratespielen ersparen.

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
