---
date: 2024-01-20 17:31:51.146303-07:00
description: "Wie geht das: Das Konzept, Daten im Voraus zu berechnen, ist nichts\
  \ Neues. Kalender und Astronomie nutzen es seit Jahrtausenden. In der Programmierung\u2026"
lastmod: '2024-04-05T22:51:08.056546-06:00'
model: gpt-4-1106-preview
summary: Das Konzept, Daten im Voraus zu berechnen, ist nichts Neues.
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

## Wie geht das:
```Python
from datetime import datetime, timedelta

# Heutiges Datum
heute = datetime.now()
print("Heute: ", heute.strftime("%d.%m.%Y"))

# 10 Tage in die Zukunft
zukunft = heute + timedelta(days=10)
print("Zukunft: ", zukunft.strftime("%d.%m.%Y"))

# 5 Tage in die Vergangenheit
vergangenheit = heute - timedelta(days=5)
print("Vergangenheit: ", vergangenheit.strftime("%d.%m.%Y"))
```

Beispielausgabe:
```
Heute:  25.03.2023
Zukunft:  04.04.2023
Vergangenheit:  20.03.2023
```

## Tiefgang:
Das Konzept, Daten im Voraus zu berechnen, ist nichts Neues. Kalender und Astronomie nutzen es seit Jahrtausenden. In der Programmierung erleichtert das `datetime`-Modul in Python die Arbeit mit Daten. Neben `timedelta` gibt es Alternativen wie `dateutil.relativedelta`, das mehr Flexibilität bietet, zum Beispiel bei der Berechnung der letzten Tag eines Monats oder beim Hinzufügen von Monaten. In der Implementierung ist es wichtig, Zeitzone und Lokalisierung zu beachten, da das Ergebnis davon abhängen kann.

## Siehe Auch:
- Python `datetime` Modul Dokumentation: https://docs.python.org/3/library/datetime.html
- `dateutil` Modul und `relativedelta` Klasse: https://dateutil.readthedocs.io/en/stable/
- Zeitzone in Python handhaben: https://pytz.sourceforge.io/
