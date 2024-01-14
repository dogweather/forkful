---
title:                "PHP: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist ein wichtiger Teil des Programmierens, um Fehler in unserem Code zu finden und zu beheben. Eine häufig verwendete Methode, um Probleme zu erkennen, ist das Drucken von Debug-Ausgaben. Aber warum sollten wir das tun?

Die Verwendung von Debug-Ausgaben hilft uns dabei, den genauen Ablauf unseres Codes zu verstehen. Dadurch können wir mögliche Fehlerursachen identifizieren und beheben. Außerdem kann es uns dabei unterstützen, komplexere Probleme zu lösen, da wir den genauen Programmfluss nachvollziehen können.

## Wie geht man vor?

Das Drucken von Debug-Ausgaben kann auf verschiedene Arten erfolgen, aber in diesem Blogbeitrag werden wir uns auf die Verwendung von `echo` und `print_r` in PHP konzentrieren.

Um mit `echo` eine Debug-Ausgabe zu erzeugen, verwenden wir einfach das Schlüsselwort gefolgt von der Variablen oder dem Wert, den wir überprüfen möchten. Dies gibt uns eine einfache textbasierte Ausgabe, die auf unserer Webseite oder in der Konsole angezeigt wird.

```PHP
echo "Debug-Ausgabe: " . $variable;
```

Bei der Verwendung von `print_r` erhalten wir eine etwas strukturiertere Ausgabe, die uns auch Arrays und Objekte anzeigt.

```PHP
print_r($array);
```

Diese beiden Methoden sind äußerst hilfreich, um schnell Informationen über unseren Code zu erhalten und Fehler zu finden.

## Tiefergehende Informationen

Beim Drucken von Debug-Ausgaben gibt es noch einige wichtige Dinge zu beachten. Zum Beispiel sollten wir darauf achten, dass wir keine sensiblen Daten auf diese Weise ausgeben, da sie für andere Benutzer einsehbar sein können.

Außerdem sollten wir nicht vergessen, Debug-Ausgaben in unserer Produktionsumgebung zu entfernen oder zumindest zu deaktivieren, um die Performance nicht zu beeinträchtigen.

Zusätzlich kann es nützlich sein, formatierte Ausgaben zu verwenden, insbesondere wenn wir mit komplexen Datenstrukturen arbeiten. Hier können wir zum Beispiel die Funktion `json_encode` oder die Methode `var_dump` verwenden, um eine besser lesbare Ausgabe zu erhalten.

## Siehe auch

- [Offizielle PHP-Dokumentation zu `echo`](https://www.php.net/manual/de/function.echo.php)
- [Offizielle PHP-Dokumentation zu `print_r`](https://www.php.net/manual/de/function.print-r.php)
- [Debug-Ausgaben richtig nutzen](https://www.php-einfach.de/experte/tutorials/debug-ausgaben-richtig-nutzen/)

Jetzt wissen Sie, warum und wie Sie Debug-Ausgaben in Ihrer PHP-Entwicklung einsetzen können. Viel Spaß beim Coden und erfolgreichen Debuggen!