---
title:    "Python: Vergleich zweier Daten"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Datumsangaben ist ein häufiger Vorgang in der Programmierung, da es uns ermöglicht, zu bestimmen, welches Datum vor oder nach dem anderen liegt. Dies ist besonders nützlich bei der Verarbeitung von Zeitdaten oder bei der Berechnung von Zeitintervallen.

# Wie man es macht

Die Vergleichsfunktion in Python ermöglicht es uns, zwei Datumsangaben miteinander zu vergleichen. Hier ist ein Beispielcode, der zeigt, wie man zwei Daten vergleicht:

``` Python
# Importiere das datetime Modul
import datetime
 
# Erstelle zwei Datumobjekte
date_1 = datetime.date(2020, 5, 12)
date_2 = datetime.date(2021, 2, 24)
 
# Vergleiche die beiden Daten
if date_1 < date_2:
    print("date_1 liegt vor date_2")
elif date_1 > date_2:
    print("date_2 liegt vor date_1")
else:
    print("Die Daten sind gleich")
```

Die Ausgabe dieses Codes wäre:

```
date_1 liegt vor date_2
```

In diesem Beispiel wird die Python-Bibliothek "datetime" verwendet, um Datumobjekte zu erstellen und mit ihnen zu arbeiten. Dann wird die "if/elif/else" -Anweisung verwendet, um die Vergleichsergebnisse zu überprüfen und die entsprechende Ausgabe zu drucken.

# Tiefer tauchen

Python ermöglicht es uns nicht nur, zwei Datumsangaben zu vergleichen, sondern wir können auch feststellen, ob ein Datum vor oder nach dem aktuellen Datum liegt. Hier ist ein Beispielcode dazu:

``` Python
# Importiere das datetime Modul
import datetime
 
# Erstelle ein Datumobjekt für heute
today = datetime.date.today()
 
# Erstellen Sie ein weiteres Datumobjekt
date = datetime.date(2021, 7, 5)
 
# Überprüfen Sie, ob das Datum in der Zukunft liegt
if date > today:
    print("Das Datum liegt in der Zukunft")
 
# Überprüfen Sie, ob das Datum in der Vergangenheit liegt
if date < today:
    print("Das Datum liegt in der Vergangenheit")
```

Die Ausgabe dieses Codes wäre:

```
Das Datum liegt in der Zukunft
```

Wenn wir die Vergleichsfunktion kombinieren und mit bedingten Anweisungen arbeiten, können wir komplexe Programme schreiben, die auf verschiedenen Datumsangaben basieren.

# Siehe auch

- [Offizielle Python-Dokumentation zu datetime](https://docs.python.org/de/3/library/datetime.html)
- [Ein Tutorial zur Arbeit mit datetime in Python](https://www.datacamp.com/community/tutorials/python-datetime-tutorial)
- [Weitere Beispiele zur Vergleichsfunktion in Python](https://thepythonguru.com/python-built-in-functions/comparison-operators/)