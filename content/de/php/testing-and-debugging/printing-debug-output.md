---
date: 2024-01-20 17:52:57.634293-07:00
description: 'How to: Hier ein einfacher Code zur Verwendung von `echo`.'
lastmod: '2024-03-13T22:44:53.974909-06:00'
model: gpt-4-1106-preview
summary: Hier ein einfacher Code zur Verwendung von `echo`.
title: Debug-Ausgaben drucken
weight: 33
---

## How to:
Hier ein einfacher Code zur Verwendung von `echo`:

```PHP
<?php
$variable = "Welt";
echo "Hallo, $variable!";
?>
```
Ausgabe: 
```
Hallo, Welt!
```

Für komplexere Datenstrukturen nutzt man `print_r` oder `var_dump`:

```PHP
<?php
$array = array('eins' => 1, 'zwei' => 2);
print_r($array);
?>
```
Ausgabe:
```
Array
(
    [eins] => 1
    [zwei] => 2
)
```

```PHP
<?php
$zahl = 42;
$wahrheit = true;
var_dump($zahl, $wahrheit);
?>
```
Ausgabe:
```
int(42) 
bool(true)
```

## Deep Dive
Druckausgabe zum Debuggen reicht weit zurück in die Programmierungsgeschichte. Früher gab's nur wenige Tools zur Fehlersuche, heute verwenden wir oft integrierte Entwicklungsumgebungen (IDEs) und komplexere Tools wie Xdebug.

Alternativen zum manuellen Drucken sind etwa Logging-Bibliotheken, die mehr Kontrolle über das Was und Wie des Loggens bieten. Logging kann in Dateien erfolgen oder externen Services wie Sentry oder Loggly.

Die Implementierung variiert je nach Sprache und Plattform. In PHP ist die Druckausgabe simpel, aber das bedeutet nicht, dass man sie überall einsetzen sollte. Zu viel Debug-Output kann die Lesbarkeit des Codes stören und die Performance senken.

## See Also
- PHP Manual on `echo`: https://www.php.net/manual/en/function.echo.php
- PHP Manual on `print_r`: https://www.php.net/manual/en/function.print-r.php
- PHP Manual on `var_dump`: https://www.php.net/manual/en/function.var-dump.php
- Xdebug, a Debugger and Profiler for PHP: https://xdebug.org/docs
- Monolog, a popular logging library for PHP: https://github.com/Seldaek/monolog
