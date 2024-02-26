---
date: 2024-01-26 04:16:18.371083-07:00
description: "Eine interaktive Shell oder REPL (Read-Eval-Print Loop) erm\xF6glicht\
  \ es Ihnen, PHP-Code spontan zu schreiben und auszuf\xFChren. Sie ist ideal f\xFC\
  r Experimente,\u2026"
lastmod: '2024-02-25T18:49:51.030279-07:00'
model: gpt-4-0125-preview
summary: "Eine interaktive Shell oder REPL (Read-Eval-Print Loop) erm\xF6glicht es\
  \ Ihnen, PHP-Code spontan zu schreiben und auszuf\xFChren. Sie ist ideal f\xFCr\
  \ Experimente,\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
Eine interaktive Shell oder REPL (Read-Eval-Print Loop) ermöglicht es Ihnen, PHP-Code spontan zu schreiben und auszuführen. Sie ist ideal für Experimente, Debugging oder Lernen, da Sie Code-Snippets testen können, ohne den Aufwand eines vollständigen Skripts zu betreiben.

## Wie geht das:
Starten Sie die PHP REPL, indem Sie `php -a` in Ihrem Terminal ausführen. Hier ist ein Vorgeschmack darauf, wie es funktioniert:

```php
php > echo "Hallo, Welt!";
Hallo, Welt!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Sie können auch Funktionen definieren:

```php
php > function summe($a, $b) { return $a + $b; }
php > echo summe(5, 10);
15
```

## Vertiefung
REPLs gibt es in irgendeiner Form seit den frühen Tagen von LISP in den 1960er Jahren. Die interaktive Shell von PHP ist weniger fortgeschritten im Vergleich zu denen von Sprachen wie Python oder JavaScript. Sie bewahrt keinen Zustand zwischen den Sitzungen und fehlt Funktionen wie Auto-Vervollständigung. Für eine funktionsreichere PHP REPL sollten Sie Alternativen wie `psysh` oder `boris` in Betracht ziehen. Diese Drittanbieter-Shells bieten bessere Introspektionswerkzeuge, Tab-Vervollständigung und sogar einen Debugger.

Unter der Haube funktioniert die REPL von PHP, indem jede eingegebene Codezeile kompiliert und ausgeführt wird. Die Grenzen dieses Ansatzes werden bei Dingen wie dem erneuten Deklarieren von Klassen klar, was in derselben Sitzung nicht möglich ist. Es ist großartig für einfache Tests, kann aber für komplexe Aufgaben umständlich werden.

## Siehe auch
- [PHP-Handbuch - Interaktive Shell](https://www.php.net/manual/de/features.commandline.interactive.php)
- [PsySH: Eine Laufzeit-Entwicklerkonsole, interaktiver Debugger und REPL für PHP](https://psysh.org/)
- [Boris: Eine winzige REPL für PHP](https://github.com/borisrepl/boris)
