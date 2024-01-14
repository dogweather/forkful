---
title:                "Fish Shell: Eine Datum in einen String umwandeln"
simple_title:         "Eine Datum in einen String umwandeln"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum in einen String kann hilfreich sein, um das Datum in einem bestimmten Format anzuzeigen oder in einer Datei zu speichern. Mit der Fish Shell können Sie dies auf einfache Weise programmatisch erreichen.

## Wie geht das?

Um ein Datum in einen String umzuwandeln, können Sie die `date` Befehlszeilenschnittstelle (CLI) oder die `strftime` Fish Shell Funktion verwenden. Hier sind Beispiele für beide Methoden mit jeweiliger Ausgabe:

```
Fish Shell Date:
date +%d.%m.%Y
30.03.2021

Fish Shell Strftime:
strftime %d.%m.%Y %m/%d/%Y
30.03.2021 03/30/2021
```

Hier sehen wir, dass die `date` CLI das Datum basierend auf dem angegebenen Format zurückgibt, während die `strftime` Funktion das Datum basierend auf dem angegebenen Format konvertiert und ausgibt.

Für weitere Optionen und Informationen zu Formaten können Sie die Dokumentation der Fish Shell oder die Manpages des `date` Befehls überprüfen.

## Tiefes Eintauchen

Eine Sache, auf die man bei der Konvertierung von Datum in einen String achten sollte, ist unterschiedliche Datumsformate in verschiedenen Ländern und Kulturen. Daher wäre es hilfreich, sich mit den ISO Standards für Datumsformatierung vertraut zu machen und diese in Ihrem Code zu berücksichtigen, um mögliche Probleme und Inkompatibilitäten zu vermeiden.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [`date` Manpage](https://man7.org/linux/man-pages/man1/date.1.html)
- [ISO Standards für Datumsformatierung](https://de.wikipedia.org/wiki/ISO_8601)