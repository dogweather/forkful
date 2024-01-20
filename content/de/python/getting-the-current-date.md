---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

---

# Datum abrufen mit Python

---

## Was & Warum?

Das Abrufen des aktuellen Datums ist ein alltäglicher Vorgang, bei dem ein Computerprogramm Informationen über das gegenwärtige Datum vom Betriebssystem erhält. Programmierer tun dies häufig, um Zeitstempel zu erstellen, Datenaufnahmen zu organisieren, oder einfach nur um den Benutzer über das aktuelle Datum zu informieren.

## Anleitung:

Python macht das Abrufen des aktuellen Datums einfach. Hier ist ein einfacher Code, der das heutige Datum in Python anzeigt:

```Python
from datetime import date

today = date.today()
print("Heutiges Datum:", today)
```

Wenn Sie diesen Code ausführen, wird das Ausgabemuster wie folgt aussehen:

```Python
Heutiges Datum: 2022-12-12
```

## Tiefere Infos:

Historisch gesehen wurden Datums- und Zeitfunktionen in many Computerprogrammen ursprünglich auf niedriger Ebene implementiert und waren oft unterschiedlich und schwierig zu handhaben. Python bietet jedoch eine hohe Abstraktion durch das datetime Modul.

Es gibt alternative Wege, das aktuelle Datum zu bekommen. Sie könnten die Zeitbibliothek verwenden und die Zeit seit der Epoche (1. Januar 1970) bekommen, und sie dann in ein Datum umwandeln.

Die Implementierungsdetails hängen vom Betriebssystem und von der Python-Installation ab. Python's datetime Bibliothek ruft die Systemzeit vom Betriebssystem ab und übersetzt sie in ein leicht zu handhabendes Datumobjekt.

## Siehe auch:

Für mehr Details:

- Python's offizielle Dokumentation auf [datetime](https://docs.python.org/3/library/datetime.html) Modul
- Ein hilfreicher Leitfaden zu [Python's Time and DateTime](https://realpython.com/python-datetime/) von Real Python
- Ein StackOverflow-Thread, der zeigt, [wie man das aktuelle Datum in Python bekommt](https://stackoverflow.com/questions/415511/how-to-get-the-current-time-in-python)

---

Mögen Ihre Codes immer fehlerfrei sein!