---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Konvertieren eines Datums in einen String ist eine Möglichkeit für Programmierer, ein Datum in einem Format darzustellen, das für Menschen leicht lesbar ist. Dies ist oft nützlich, um Daten in einer benutzerfreundlichen Weise zu präsentieren oder um Daten in einer Datenbank zu speichern.

## So geht's:

Der Befehl `date` wird in Fish Shell verwendet, um ein Datum in eine Zeichenkette zu konvertieren. Hier ist ein einfacher Beispielcode:

```fish
# Aktuelles Datum und Zeit anzeigen
set datum (date)
echo $datum
```

Ausführung dieses Codes liefert etwas Ähnliches wie:
```fish
Thu 23 Dec 2021 10:21:27 PM CET
```

Jetzt, wenn Sie ein spezifisches Format wünschen, können Sie es wie folgt tun:
```fish
# Datum und Zeit im Format DD-MM-YYYY anzeigen
set datum (date "+%d-%m-%Y")
echo $datum
```

Ausführung dieses Codes liefert:
```fish
23-12-2021
```

## Tiefentauchen:

Fish Shell ist eine moderne Shell, die Anfang der 2000er Jahre entwickelt wurde. Sie baut auf den Erfahrungen und Einsichten auf, die in früheren Shells gewonnen wurden, einschließlich des Konzepts der Datums-/Zeitkonvertierung in eine Zeichenkette.

Als Alternative zur Fish Shell gibt es andere Shells wie die Bourne Shell (sh), die C Shell (csh), die Korn Shell (ksh) und die Bourne-Again Shell (bash) die ebenfalls `date` verwenden können, um das Datum zu konvertieren.

Die Implementierungsdetails des `date`-Befehls in der Fish Shell sind ziemlich komplex, aber im wesentlichen ruft der Befehl die Systemzeit ab, konvertiert sie in eine Zeichenkette und formatiert sie dann entsprechend den vorgegebenen Parameter.

## Siehe auch:

- Offizielle Fish Shell Dokumentation über `date`: [https://fishshell.com/docs/current/commands.html#date](https://fishshell.com/docs/current/commands.html#date)
- Weitere Informationen zur Datums- und Zeitkonvertierung: [https://de.wikipedia.org/wiki/Datums-_und_Zeitangaben_in_Informatik](https://de.wikipedia.org/wiki/Datums-_und_Zeitangaben_in_Informatik)