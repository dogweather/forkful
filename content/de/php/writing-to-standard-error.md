---
title:    "PHP: Schreiben auf Standardfehler"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben in die Standardfehlerausgabe kann für PHP Programmierer aus verschiedenen Gründen nützlich sein. Zum Beispiel hilft es bei der Fehlerbehandlung, bei der Ausgabe von Informationsmeldungen oder beim Debugging von Code.

## Wie geht man vor

Um in die Standardfehlerausgabe zu schreiben, verwendet man die Funktion `error_log()`. Hier ist ein Beispiel, wie man eine Fehlermeldung mit dieser Funktion ausgeben kann:

```PHP
<?php
// Fehlermeldung ausgeben
$errorMsg = "Es ist ein Problem aufgetreten!";
error_log($errorMsg);
```

Dieser Codeblock generiert eine Fehlermeldung, die in der Standardfehlerausgabe angezeigt wird. Man kann auch andere Parameter angeben, wie z.B. einen Pfad zu einer Log-Datei, in die die Fehlermeldung geschrieben werden soll.

## Tiefere Einblicke

Es gibt verschiedene Gründe, warum das Schreiben in die Standardfehlerausgabe nützlich ist. Zum Beispiel können Entwickler damit leichter Fehler in ihrem Code finden, da alle Fehlermeldungen an einem Ort gesammelt werden. Außerdem können Entwickler Spezifikationen oder Informationen über den Code oder die Anwendung ausgeben, die für die Fehlerbehandlung oder das Debugging hilfreich sein können.

Ein weiterer Grund ist, dass man das Schreiben in die Standardfehlerausgabe auch in Kombination mit anderen Funktionen nutzen kann. Zum Beispiel kann man beim Schreiben in die Standardfehlerausgabe auch die `die()` Funktion verwenden, um den Code anzuhalten und eine spezifische Fehlermeldung auszugeben.

## Siehe auch

- [PHP Dokumentation über error_log()](https://www.php.net/manual/de/function.error-log.php)
- [Beispiel für die Verwendung von error_log()](https://www.geeksforgeeks.org/php-error_log-function/)
- [Ausführliche Erklärung von error_log() mit Beispielen](https://www.w3schools.com/php/func_error_log.asp)