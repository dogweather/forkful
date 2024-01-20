---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Datum zu Zeichenkette in Lua konvertieren

## Was & Warum?

Die Konvertierung eines Datums in eine Zeichenkette ermöglicht es uns, Daten in einem menschenlesbaren Format zu präsentieren. Programmierer machen das, um die Benutzerfreundlichkeit von Anwendungen zu verbessern.

## So geht's:

In Lua können wir das `os.date` Modul verwenden, um ein Datum in eine Zeichenkette zu konvertieren.

```Lua
datum = os.date("%Y-%m-%d")
print(datum)
```

Die Ausgabe dieses Code-Beispiels würde das aktuelle Datum in ISO-Format ausgeben, zum Beispiel:

```
2023-09-15
```

## Vertiefung

Die `os.date` Funktion in Lua stammt ursprünglich aus der C-Programmiersprache und ist seit Lua 5.1 vorhanden. Als Alternativen könnten andere Konstruktionen wie `string.format` verwendet werden, dies erfordert jedoch zusätzliche Schritte zur Umwandlung des Datums in numerische Formate.

Die Implementierungsdetails der `os.date` Funktion können von der zugrunde liegenden Plattform abhängen. Sie verwendet in der Regel die Systemzeit zur Generierung des Datums und kann auch mit unterschiedlichen Zeitzonen umgehen.

## Siehe auch

- [Lua `os.date` Dokumentation](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Formatierende Zeit und Datum in Lua](https://riptutorial.com/lua/example/20310/formatting-date-and-time)
- [Einführung in Lua](http://learncodethehardway.org/lua/)