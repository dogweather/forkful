---
title:    "Python: Vergleich zweier Daten"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Datumsangaben ist eine häufige Aufgabe in der Programmierung. Es ermöglicht uns, bestimmte Aktionen basierend auf dem Datum auszuführen, wie zum Beispiel das Überprüfen von Verfallsdaten oder das Sortieren von Daten nach Datum. In dieser Blog-Post werden wir uns ansehen, wie man in Python zwei Datumswerte vergleicht.

# Wie geht das?

Um zwei Datumswerte in Python zu vergleichen, können wir den Vergleichsoperator "==" verwenden. Dieser Operator prüft, ob zwei Werte gleich sind und gibt entweder "True" oder "False" zurück, je nachdem, ob die Bedingung erfüllt ist oder nicht.

```python
# Vergleich von zwei Datumswerten
date_1 = "2021-01-01"
date_2 = "2021-01-02"
if date_1 == date_2:
    print("Die beiden Datumsangaben sind gleich.")
else:
    print("Die beiden Datumsangaben sind nicht gleich.")
```

Die Ausgabe dieses Codes wird sein: "Die beiden Datumsangaben sind nicht gleich." Da die beiden Datumsangaben unterschiedlich sind, gibt der Vergleichsoperator "==" "False" zurück.

Um jedoch zu vermeiden, dass manuell zwei Datumswerte eingegeben werden müssen, können wir auch die Python-Bibliothek "datetime" verwenden, um zwei Datumswerte zu erstellen und zu vergleichen.

```python
from datetime import date
# Erstellung der Datumswerte
date_1 = date(2021, 1, 1)
date_2 = date(2021, 1, 2)
if date_1 == date_2:
    print("Die beiden Datumsangaben sind gleich.")
else:
    print("Die beiden Datumsangaben sind nicht gleich.")
```

Die Ausgabe dieses Codes ist dieselbe wie zuvor, jedoch basiert sie nun auf den von uns erstellten Datumswerten mit Hilfe der "datetime"-Bibliothek.

# Nahaufnahme

Beim Vergleichen von zwei Datumswerten ist es wichtig, dass beide Werte im gleichen Format vorliegen. Ansonsten könnte es zu unerwarteten Ergebnissen führen. Zum Beispiel werden die folgenden beiden Datumsangaben unterschiedliche Ergebnisse zurückgeben, da das Format nicht übereinstimmt.

```python
date_1 = "2021-01-01"
date_2 = "01/01/2021"
```

Es ist auch wichtig zu beachten, dass der Vergleich der Datumsangaben auch die Zeit mit einbeziehen kann, wenn diese in den jeweiligen Werten enthalten ist. In diesem Fall muss das Format der Zeit ebenfalls übereinstimmen, ansonsten kann es gegebenenfalls zu inkonsistenten Ergebnissen kommen.

# Siehe auch

- [Python-Dokumentation zu Datum und Zeit](https://docs.python.org/de/3/library/datetime.html)
- [Python-Dokumentation zur Vergleichsoperatoren](https://docs.python.org/de/3/library/operator.html#comparison-operators)