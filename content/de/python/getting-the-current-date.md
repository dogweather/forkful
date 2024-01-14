---
title:    "Python: Die aktuelle Datum erhalten"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe bei der Programmierung in Python. Es kann hilfreich sein, um zeitabhängige Funktionen oder Prozesse zu steuern oder einfach um das aktuelle Datum in einem bestimmten Format anzuzeigen.

# Wie man es macht

Die Python-Standardbibliothek bietet eine integrierte Funktion `date.today()`, die das heutige Datum in einem `date`-Objekt zurückgibt. Dieses Datum kann dann in verschiedenen Formaten angezeigt werden. Hier ist ein Beispiel, wie man das aktuelle Datum als String im Format "Tag/Monat/Jahr" ausgeben kann:

```Python
from datetime import date

heute = date.today()
formatiertes_datum = heute.strftime("%d/%m/%y")

print(formatiertes_datum)
```

Das Ergebnis dieses Codes ist `21/05/21`, abhängig von dem Tag, an dem Sie es ausführen.

# Tiefere Einblicke

Es gibt verschiedene Funktionen und Methoden in der `datetime`-Bibliothek, die es ermöglichen, das aktuelle Datum auf unterschiedliche Weise darzustellen und zu manipulieren. Zum Beispiel kann man mit `date.weekday()` den Wochentag des aktuellen Datums als Zahl (0 für Montag, 6 für Sonntag) oder als Namen (z.B. "Montag") abrufen. Mit der Funktion `date.fromisoformat()` kann man ein Datum aus einem String im ISO-Format erstellen.

Es gibt auch Möglichkeiten, mit den Daten und Zeiten in Python zu rechnen. Zum Beispiel kann man mit `date.isleap()` überprüfen, ob das aktuelle Jahr ein Schaltjahr ist. Oder mit `date.replace()` ein Datum ändern, z.B. den Tag, den Monat oder das Jahr.

Es lohnt sich, sich in die Dokumentation der `datetime`-Bibliothek einzulesen, um alle Möglichkeiten kennen zu lernen.

# Siehe auch

- [Offizielle Dokumentation zur datetime-Bibliothek](https://docs.python.org/3/library/datetime.html)
- [Tutorial zum Arbeiten mit Datum und Zeit in Python](https://realpython.com/python-datetime/)
- [Stack Overflow: Wie bekomme ich das aktuelle Datum in Python?](https://stackoverflow.com/questions/415511/how-to-get-the-current-time-in-python)