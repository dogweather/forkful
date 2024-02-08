---
title:                "Datum in einen String umwandeln"
aliases:
- de/bash/converting-a-date-into-a-string.md
date:                  2024-01-20T17:35:51.195648-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln eines Datums in einen String bedeutet, ein Datum in Textform auszudrücken. Programmierer nutzen dies für eine bessere Lesbarkeit und um Daten in Log-Dateien, Berichten oder Benutzeroberflächen anzuzeigen.

## Vorgehensweise:
Bash bietet `date`, ein mächtiges Tool für Datum und Zeit. Hier ist, wie man es verwendet:

```Bash
# Aktuelles Datum und Uhrzeit als String
date "+%Y-%m-%d %H:%M:%S"

# Beispiel-Ausgabe
2023-04-01 12:00:01

# Nur das Datum
date "+%Y-%m-%d"

# Beispiel-Ausgabe
2023-04-01
```

Du kannst das Format anpassen, indem du die Platzhalter änderst. `%Y` für das Jahr, `%m` für den Monat, usw.

```Bash
# Ein benutzerdefiniertes Format
date "+%d.%m.%Y - %H:%M"

# Beispiel-Ausgabe
01.04.2023 - 12:00
```

## Tiefgang:
Die `date`-Kommandozeilenfunktion gibt's schon lange, Teil des GNU coreutils-Pakets. Alternativen inkludieren Sprachen wie `Python` oder `Perl`, die komplexere Datumsmanipulationen erlauben. In Bash kann das Datumsformat mit den `strftime` Platzhaltern nahezu beliebig angepasst werden, wodurch du die Ausgabe deinen Anforderungen entsprechend gestalten kannst.

Wichtige Punkte zur Implementierung:
- Bash selbst hat keine eingebauten Funktionen zur Datumsmanipulation; `date` ist ein externes Programm.
- Beachte die Zeitzone, `date` zeigt das Datum in der aktuellen Zeitzone des Systems.
- Für wiederkehrende Aufgaben können Funktionen in Bash-Skripts genutzt werden, um den Umgang mit Datum und Zeit zu vereinfachen.

## Siehe auch:
- GNU coreutils: https://www.gnu.org/software/coreutils/
- Bash Dateiformatierung mit `strftime`: https://www.man7.org/linux/man-pages/man3/strftime.3.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/datesandtimes.html

Während dieser Artikel einen kurzen Überblick bietet, kann die Vertiefung in die Dokumentation und weiterführende Ressourcen dir helfen, ein echter Datum-Zauberer in Bash zu werden!
