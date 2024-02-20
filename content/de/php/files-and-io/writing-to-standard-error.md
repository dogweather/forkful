---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:03.820693-07:00
description: "Das Schreiben auf den Standardfehler (stderr) in PHP bedeutet, Fehlermeldungen\
  \ oder Diagnosen getrennt von der Standardausgabe (stdout) zu leiten, was\u2026"
lastmod: 2024-02-19 22:05:12.919421
model: gpt-4-0125-preview
summary: "Das Schreiben auf den Standardfehler (stderr) in PHP bedeutet, Fehlermeldungen\
  \ oder Diagnosen getrennt von der Standardausgabe (stdout) zu leiten, was\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf den Standardfehler (stderr) in PHP bedeutet, Fehlermeldungen oder Diagnosen getrennt von der Standardausgabe (stdout) zu leiten, was Entwicklern hilft, ihre Ausgabeströme für das Debugging und Protokollieren besser zu verwalten. Programmierer nutzen diese Technik, um sicherzustellen, dass Fehlermeldungen nicht mit der Ausgabe des Programms interferieren, was die Überwachung und Fehlersuche in Anwendungen erleichtert.

## Wie geht das:

In PHP kann das Schreiben auf stderr mit der Funktion `fwrite()` in Verbindung mit der vordefinierten Konstante `STDERR`, die den Fehlerausgabestrom repräsentiert, erreicht werden.

```php
<?php
// Eine einfache Nachricht auf stderr schreiben.
fwrite(STDERR, "Das ist eine Fehlermeldung.\n");
```

Beispielausgabe, wenn das Skript von der Kommandozeile ausgeführt wird:
```
Das ist eine Fehlermeldung.
```

Um eine praxisnähere Anwendung zu demonstrieren, betrachten Sie ein Szenario, in dem Sie Benutzereingaben parsen und auf unerwartete Daten stoßen:
```php
<?php
$input = 'unerwartete Daten';

// Simulation eines Fehlers bei der Verarbeitung der Benutzereingabe.
if ($input === 'unerwartete Daten') {
    fwrite(STDERR, "Fehler: Unerwartete Eingabe erhalten.\n");
    exit(1); // Beenden mit einem Nicht-Null-Wert um einen Fehler anzuzeigen.
}
```

Obwohl die in PHP eingebauten Funktionen zur Behandlung von stderr in der Regel ausreichend sind, können bei komplexeren Anwendungen oder dem Wunsch, die stderr-Protokollierung mit externen Systemen zu integrieren, Drittanbieterbibliotheken wie Monolog ein mächtiger Verbündeter sein. Monolog ist eine Protokollierungsbibliothek, die stderr unter vielen anderen Zielen (Dateien, Sockets usw.) verarbeiten kann.

Verwendung von Monolog, um auf stderr zu schreiben:

Zuerst stellen Sie sicher, dass Sie Monolog über Composer installiert haben:
```
composer require monolog/monolog
```

Dann können Sie Monolog so konfigurieren, dass es den `StreamHandler` verwendet, der auf `php://stderr` ausgerichtet ist:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Erstellen eines Log-Kanals
$log = new Logger('name');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Eine Log-Nachricht zu stderr hinzufügen
$log->warning('Das ist eine Warnmeldung.');
```

Der obige Code verwendet Monolog, um eine Warnmeldung an stderr zu senden, was insbesondere für Anwendungen nützlich ist, die detaillierte Protokollierungskonfigurationen oder externe Protokollüberwachungen benötigen.
