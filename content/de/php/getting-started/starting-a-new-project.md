---
date: 2024-01-20 18:04:08.885079-07:00
description: "Ein neues Projekt starten bedeutet, eine frische Codebasis aufzubauen.\
  \ Programmierer machen das, um Ideen in funktionierende Software umzusetzen und\u2026"
lastmod: '2024-03-13T22:44:53.973070-06:00'
model: gpt-4-1106-preview
summary: "Ein neues Projekt starten bedeutet, eine frische Codebasis aufzubauen. Programmierer\
  \ machen das, um Ideen in funktionierende Software umzusetzen und\u2026"
title: Einen neuen Projekt starten
weight: 1
---

## Was & Warum?
Ein neues Projekt starten bedeutet, eine frische Codebasis aufzubauen. Programmierer machen das, um Ideen in funktionierende Software umzusetzen und Probleme zu lösen.

## How to:
```PHP
<?php
// Ein neues PHP-Projekt beginnen:
echo "Hallo Welt!";

// Composer für Dependency Management verwenden:
// composer.json Datei erstellen
{
    "require": {
        "monolog/monolog": "2.0"
    }
}

// In der Konsole: composer install

// Autoloader verwenden, um eine Bibliothek zu laden
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Einen Logger erstellen
$log = new Logger('name');
$log->pushHandler(new StreamHandler('app.log', Logger::WARNING));

// Eine Warnung ins Log schreiben
$log->warning('Achtung, etwas könnte schiefgehen!');
?>
```
Ausgabe:
```
Hallo Welt!
```
## Deep Dive
Anfänge mit PHP reichen zurück in das Jahr 1995; heute ist PHP 8.x die aktuelle Version. Composer ist der De-facto-Standard für das Management von Abhängigkeiten in PHP-Projekten. Früher mussten Bibliotheken manuell eingebunden werden, was aufwendig und fehleranfällig war. Composer nutzt `composer.json`, wo Abhängigkeiten festgehalten sind, lädt sie herunter und stellt einen Autoloader bereit.

Für größere Projekte gibt es Frameworks wie Laravel oder Symfony, die weit mehr als nur ein Autoloader bieten: Sicherheit, Routing, Templates und mehr. Diese nehmen dir viel Grundlagenarbeit ab, sind aber nicht immer nötig. Kleinere Projekte oder einfache Scripte können ohne Frameworks schnell und direkt umgesetzt werden.

Die Umsetzung fängt immer mit einem klaren Ziel vor Augen an. Planung, Strukturierung und das Festlegen von Anforderungen sind entscheidend, um ein Projekt erfolgreich aufzubauen und zu warten. Skalierbarkeit und Wartbarkeit stehen im Fokus: Besser ein sauberes Fundament legen als später das ganze Gebäude wegen morscher Balken abreißen zu müssen.

## See Also
- PHP-The Right Way: https://phptherightway.com
- Composer: https://getcomposer.org
- PHP Frameworks Vergleich: https://www.phpbenchmarks.com/de/vergleich/frameworks
