---
date: 2024-01-26 01:07:27.182287-07:00
description: "Logging ist im Grunde genommen wie das F\xFChren eines Tagebuchs f\xFC\
  r Ihren Code; es ist der Akt des Aufzeichnens von Ereignissen, Fehlern und anderen\u2026"
lastmod: '2024-03-11T00:14:27.877337-06:00'
model: gpt-4-1106-preview
summary: "Logging ist im Grunde genommen wie das F\xFChren eines Tagebuchs f\xFCr\
  \ Ihren Code; es ist der Akt des Aufzeichnens von Ereignissen, Fehlern und anderen\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?

Logging ist im Grunde genommen wie das Führen eines Tagebuchs für Ihren Code; es ist der Akt des Aufzeichnens von Ereignissen, Fehlern und anderen bedeutsamen Datenpunkten, die auftreten, wenn Ihre Anwendung läuft. Programmierer machen dies, um den Überblick darüber zu behalten, was unter der Haube geschieht, Probleme zu debuggen und eine Prüfspur für spätere Analysen oder zur Einhaltung von Compliance-Anforderungen zu pflegen.

## Wie geht das:

PHP verfügt über eine integrierte Fehlerprotokollierungsfunktion, die einfach zu verwenden ist. Fügen Sie einfach `error_log()` in Ihren Code ein, um eine Nachricht an Ihre Server-Logs zu senden. Sie können es auch so anpassen, dass es in eine bestimmte Datei schreibt.

```php
<?php
// Protokollieren einer einfachen Informationsnachricht
error_log("Dies ist ein Info-Log-Eintrag.");

// Protokollieren einer Fehlermeldung
error_log("Dies ist ein Fehler-Log-Eintrag.", 0);

// Protokollieren in eine angegebene Datei
file_put_contents('/Pfad/zur/deiner/custom.log', "Ein benutzerdefinierter Log-Eintrag.\n", FILE_APPEND);

// Verwendung von Monolog für strukturierte Protokollierung
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Erstellen des Loggers
$logger = new Logger('name');
// Jetzt einige Handler hinzufügen
$logger->pushHandler(new StreamHandler('/Pfad/zur/deinem/monolog.log', Logger::WARNING));

// Jetzt können Sie Ihren Logger verwenden
$logger->warning('Das ist ein Warn-Log!');
$logger->error('Das ist ein Fehler-Log!');
?>
```

Dies wird Ihre Protokolle entweder in das Serverprotokoll oder in Ihre angegebene Datei im Klartextformat ausgeben.

## Vertiefung:

Früher verließen sich PHP-Entwickler auf die `error_log()` Funktion oder die Apache/Nginx-Logs, um Probleme zu erfassen, aber das kann chaotisch sein mit der Notwendigkeit, Klartextdateien zu parsen und sie nicht leicht filtern oder sortieren zu können. Hier kommen Protokollierungsbibliotheken wie Monolog ins Spiel, die das Zeitalter des strukturierten Loggings in PHP eingeleitet haben. Diese Lösungen geben Ihnen eine bessere Kontrolle, indem sie mehrere Protokollierungskanäle, Schweregrade und formatierte Ausgaben (wie JSON, was ein Traum für die programmatische Analyse ist) anbieten.

Alternativen zu Monolog umfassen Log4php, KLogger und Apache's Log4php. In Bezug auf die Implementierung erfordert robustes Logging nicht nur das beliebige Ablegen von Daten, sondern berücksichtigt Dinge wie Protokollrotation, Archivierungsstrategien und die Integration mit Überwachungswerkzeugen, um wirklich nützlich zu sein.

Sie sollten die [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/) im Kopf behalten, die eine gemeinsame Schnittstelle für Protokollierungsbibliotheken definiert und die Interoperabilität und eine konsistente Art und Weise, auf Protokollierungsmechanismen zuzugreifen, gewährleistet.

## Siehe auch:

- [Monolog GitHub-Repository](https://github.com/Seldaek/monolog)
- [PSR-3 Logger Interface Spezifikation](https://www.php-fig.org/psr/psr-3/)
- [PHP-Fehlerprotokoll-Dokumentation](https://www.php.net/manual/de/function.error-log.php)
- [KLogger: Eine einfache Logging-Klasse für PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Ein vielseitiges Protokollierungsframework für PHP](https://logging.apache.org/log4php/)

Versuchen Sie es zunächst mit den eingebauten Funktionen, aber für einen wartbareren und skalierbareren Ansatz sollten Sie in Erwägung ziehen, sich mit einer Bibliothek wie Monolog vertraut zu machen. Frohes Loggen!
