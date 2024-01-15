---
title:                "Schreiben auf die Standardfehlerausgabe"
html_title:           "PHP: Schreiben auf die Standardfehlerausgabe"
simple_title:         "Schreiben auf die Standardfehlerausgabe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Manchmal kann es vorkommen, dass wir in unseren PHP-Programmen Fehlermeldungen oder andere wichtige Informationen ausgeben möchten. Standardmäßig werden diese jedoch auf dem Bildschirm angezeigt, was nicht immer ideal ist. Deshalb ist es wichtig zu wissen, wie man in PHP Informationen an den sogenannten Standardfehler (standard error) ausgeben kann.

## Wie geht das?

Um Informationen an den Standardfehler auszugeben, können wir die Funktion ```fwrite()``` verwenden. Diese Funktion erwartet zwei Parameter: Zuerst den Datei-Handle von ```STDERR```, das für den Standardfehler steht, und dann die zu schreibende Nachricht als String.

```PHP
fwrite(STDERR, 'Eine wichtige Fehlermeldung');
```
Dieser Code wird die Nachricht "Eine wichtige Fehlermeldung" an den Standardfehler ausgeben.

Eine andere Möglichkeit ist die Verwendung von ```error_log```. Diese Funktion ermöglicht es uns, Informationen direkt an den Standardfehler zu senden, ohne einen Datei-Handle zu öffnen.

```PHP
error_log('Das ist eine wichtige Information', 0);
```

In beiden Fällen wird die Ausgabe nicht auf dem Bildschirm angezeigt, sondern in einem separaten Log-File gespeichert.

## Deep Dive

Standardmäßig ist der Standardfehler in PHP auf die gleiche Log-Datei wie der Standardausgang (standard output) eingestellt. Diese Einstellung kann jedoch durch das Setzen von ```ini_set('display_errors', 0);``` geändert werden. Dadurch werden die Fehlermeldungen nicht mehr auf dem Bildschirm ausgegeben, sondern nur noch in der Log-Datei.

Eine weitere Möglichkeit ist das Umlenken des Standardfehlers in eine eigene Log-Datei. Dies kann erreicht werden, indem man den Datei-Handle von ```STDERR``` auf eine Datei setzt, in die die Fehlermeldungen geschrieben werden sollen.

```PHP
$file = fopen('error.log', 'a');
fwrite(STDERR, 'Dies ist eine wichtige Fehlermeldung');
```

Dieser Code wird die Fehlermeldung in die Datei "error.log" schreiben, anstatt sie auf dem Bildschirm anzuzeigen.

## Siehe auch

- [PHP fwrite() Funktion] (https://www.php.net/manual/de/function.fwrite.php)
- [PHP error_log() Funktion] (https://www.php.net/manual/de/function.error-log.php)
- [PHP Standardimput/-ausgang (STDOUT/STDERR)] (https://www.php.net/manual/de/faq.commandline.php#faq.commandline.std)
- [PHP ini_set() Funktion] (https://www.php.net/manual/de/function.ini-set.php)