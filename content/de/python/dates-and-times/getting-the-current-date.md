---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:27.296648-07:00
description: "Wie geht das: **Verwendung der Standardbibliothek `datetime`:** Das\
  \ `datetime` Modul in Pythons Standardbibliothek bietet Klassen f\xFCr die Manipulation\
  \ von\u2026"
lastmod: '2024-04-05T21:53:55.344797-06:00'
model: gpt-4-0125-preview
summary: "**Verwendung der Standardbibliothek `datetime`:** Das `datetime` Modul in\
  \ Pythons Standardbibliothek bietet Klassen f\xFCr die Manipulation von Daten und\
  \ Zeiten."
title: Den aktuellen Datum abrufen
weight: 29
---

## Wie geht das:
**Verwendung der Standardbibliothek `datetime`:**

Das `datetime` Modul in Pythons Standardbibliothek bietet Klassen für die Manipulation von Daten und Zeiten. Um das aktuelle Datum zu erhalten, können Sie die Methode `date.today()` verwenden.

```python
from datetime import date

today = date.today()
print(today)  # Ausgabe: JJJJ-MM-TT (z.B. 2023-04-05)
```

**Zeitformatierung:**

Wenn Sie das aktuelle Datum in einem anderen Format benötigen, ermöglicht Ihnen die Methode `strftime`, ein benutzerdefiniertes Datumsformat anzugeben:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Beispiel-Format: "April 05, 2023"
print(formatted_date)
```

**Verwendung von `pendulum` für mehr Flexibilität (eine beliebte Drittanbieter-Bibliothek):**

`Pendulum` ist eine Drittanbieter-Bibliothek, die einen intuitiveren Ansatz zum Umgang mit Daten und Zeiten in Python bietet. Sie erweitert die Standardfunktionalitäten von datetime und vereinfacht u.a. die Verwaltung von Zeitzonen.

Stellen Sie zunächst sicher, dass Sie `pendulum` über pip installiert haben:

```shell
pip install pendulum
```

Um dann das aktuelle Datum zu erhalten:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Ausgabe: JJJJ-MM-TT (z.B. 2023-04-05)
```

Mit `pendulum` ist die Formatierung ebenfalls unkompliziert und ähnlich dem `strftime`-Ansatz:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # Standardformat: "Apr 5, 2023"
print(formatted_date)
```
