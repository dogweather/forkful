---
title:    "Python: Ausgabe von Debug-Informationen"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind ein nützliches Tool beim Programmieren, da sie dabei helfen können, Fehler in einem Code zu lokalisieren und zu beheben. Durch das Ausgeben bestimmter Werte oder Zwischenergebnisse im Code kann man sich einen Überblick über den Programmablauf verschaffen und potenzielle Fehlerquellen identifizieren.

## Wie man Debug-Ausgaben erstellt

Um Debug-Ausgaben zu erstellen, kann man in Python die Funktion `print()` verwenden. Dabei können verschiedene Datentypen wie Strings, Zahlen oder Variablen ausgegeben werden. Hier ist ein Beispiel:

```Python
# Beispielcode
name = "Max"
age = 25
print("Name:", name)
print("Alter:", age)
print("Vollständige Ausgabe:", name, "ist", age, "Jahre alt.")
```

Die Ausgabe dieses Codes sieht folgendermaßen aus:

```
Name: Max
Alter: 25
Vollständige Ausgabe: Max ist 25 Jahre alt.
```

So können beispielsweise Variablenwerte oder Zwischenstände im Code ausgegeben werden, um den Programmablauf zu überprüfen.

## Tiefere Einblicke

Es gibt noch weitere Möglichkeiten, um Debug-Ausgaben zu erstellen und diese zu formatieren. Mit der Funktion `format()` können beispielsweise Variablenwerte in einen String eingefügt werden, um die Ausgabe übersichtlicher zu gestalten. Auch das Modul `pprint` bietet hilfreiche Funktionen, um komplexe Datenstrukturen anschaulicher auszugeben.

Für fortgeschrittene Nutzer gibt es auch die Möglichkeit, ein Logging-System einzurichten, um Debug-Ausgaben in Dateien zu schreiben und diese somit längerfristig zu speichern und zu analysieren.

## Siehe auch

- [Python Debugging with print Statements](https://www.google.com)
- [Python Logging Library](https://www.google.com)
- [The pprint module](https://www.google.com)