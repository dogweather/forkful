---
date: 2024-01-26 04:44:33.506016-07:00
description: "Komplexe Zahlen sind eine Menge von Zahlen der Form `a + bi`, wobei\
  \ `a` und `b` reelle Zahlen sind und `i` die imagin\xE4re Einheit ist (`i^2 = -1`).\
  \ In der\u2026"
lastmod: '2024-03-11T00:14:27.335429-06:00'
model: gpt-4-0125-preview
summary: "Komplexe Zahlen sind eine Menge von Zahlen der Form `a + bi`, wobei `a`\
  \ und `b` reelle Zahlen sind und `i` die imagin\xE4re Einheit ist (`i^2 = -1`).\
  \ In der\u2026"
title: Umgang mit komplexen Zahlen
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen sind eine Menge von Zahlen der Form `a + bi`, wobei `a` und `b` reelle Zahlen sind und `i` die imaginäre Einheit ist (`i^2 = -1`). In der Programmierung verwenden wir sie, um Probleme in verschiedenen Bereichen zu lösen, wie in der Elektrotechnik, der Signalverarbeitung und dem Quantencomputing.

## Wie:
Python bietet eine eingebaute Unterstützung für komplexe Zahlen. Hier ist, wie man mit ihnen arbeiten kann:

```Python
# Erstellen komplexer Zahlen
z = 4 + 5j
print(z)  # Ausgabe: (4+5j)

# Zugriff auf den Real- und Imaginärteil
print(z.real)  # Ausgabe: 4.0
print(z.imag)  # Ausgabe: 5.0

# Komplexe Arithmetik
w = 1 - 2j
print(z + w)  # Ausgabe: (5+3j)
print(z - w)  # Ausgabe: (3+7j)
print(z * w)  # Ausgabe: (14+2j)
print(z / w)  # Ausgabe: (-3.6+1.2j)

# Betrag (absoluter Wert)
print(abs(z))  # Ausgabe: 6.4031242374328485

# Konjugiert einer komplexen Zahl
print(z.conjugate())  # Ausgabe: (4-5j)
```

## Vertiefung
Komplexe Zahlen wurden erstmals im 16. Jahrhundert von Gerolamo Cardano konzeptualisiert. Python behandelt, wie auch andere Programmiersprachen, komplexe Zahlen als Bürger erster Klasse. Das bedeutet, sie sind in die Sprache eingebaut, mit einfach zu verwendenden Funktionen, sodass das Importieren externer Bibliotheken für Grundoperationen nicht notwendig ist.

Für umfangreiche numerische Berechnungen verfügt Python jedoch über eine Bibliothek namens `cmath`, die speziell für komplexe Zahlen ist. Sie verfügt über zusätzliche Funktionen wie `exp`, `log` und trigonometrische Operationen.

Wenn Python nicht ausreicht, könnte man sich Bibliotheken wie NumPy zuwenden, insbesondere für Array-Operationen mit komplexen Zahlen. NumPy bietet optimierte und vektorisierte Operationen, die für die Leistung in der numerischen Berechnung entscheidend sind.

## Siehe auch
Schauen Sie sich diese Ressourcen an, um mehr zu erfahren:

- Die offizielle Python-Dokumentation zu komplexen Zahlen: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Die Dokumentation des `cmath`-Moduls: https://docs.python.org/3/library/cmath.html
- NumPy für den Umgang mit Arrays komplexer Zahlen: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
