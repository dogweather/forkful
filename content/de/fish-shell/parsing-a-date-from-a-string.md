---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was und Warum?
Das Parsen eines Datums aus einem String bezeichnet den Prozess der Umwandlung des Zeichenkettenformats eines Datums in ein tatsächliches Datumsobjekt, das von einem Programm verwendet werden kann. Programmierer machen das häufig, weil viele Systeme und Datenformate Daten in Zeichenkettenform zurückgeben.

## So geht's:
Sie können den eingebauten "date" Befehl in der Fish Shell verwenden, um ein Datums-String zu parsen. Hier ist ein einfaches Beispiel, wie man das macht:

```Fish Shell
# Definiere einen Datum-String
string Datum="2022-03-14"

# Parse das Datum mit dem "date"-Befehl
date -d $Datum
```

Der obige Code gibt das geparste Datum in der Form "Mon DD HH:MM:SS CET YYYY" aus.

```Fish Shell
# Beispiel Ausgabe
Mon Mar 14 00:00:00 CET 2022
```

## Deep Dive:

- Historischer Kontext: Der Gebrauch von Datumsparsing ist seit der Anfangszeit von UNIX weithin bekannt. Der "date"-Befehl, den wir in Unix-ähnlichen Systemen (wie die Fish Shell) sehen, hat seine Wurzeln in den 1970er Jahren.

- Alternativen: Obwohl der "date"-Befehl universell genutzt wird, gibt es andere Tools wie "datetime" in Python, die ausgefeiltere Funktionen wie Zeitzonenumwandlungen und Zeitspannenberechnungen bieten.

- Implementierungsdetails: Der "date"-Befehl in der Fish Shell verwendet die glibc-Funktion "strptime" für das Parsen von Datum-Strings. Sie bietet eine Vielzahl von Möglichkeiten, das Format Ihres Datum-Strings anzugeben.

## Siehe auch:

- Offizielle Dokumentation für den "date"-Befehl (https://linux.die.net/man/1/date)

- Fish Shell GitHub Seite (https://github.com/fish-shell/fish-shell)

- Fish Shell Programmierhandbuch (https://fishshell.com/docs/current/index.html)