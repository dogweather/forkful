---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "Python: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum?
Die Konvertierung eines Strings in Kleinbuchstaben ist ein häufig vorkommendes Konzept in der Programmierung. Es bezieht sich einfach darauf, alle Buchstaben in einem String in kleinere, nicht-kapitale Buchstaben zu verwandeln. Programmierer tun dies oft, um Strings auf einfache Weise zu vergleichen, da Groß- und Kleinschreibung oft bei Vergleichen ignoriert wird.

# Wie geht's?
Um einen String in Kleinbuchstaben zu konvertieren, gibt es eine integrierte Funktion in Python namens "lower()". Hier ist ein Beispiel, wie Sie diese Funktion verwenden können:

```python
text = "HALLO WELT"
print(text.lower())
```

Das obenstehende Skript wird "hallo welt" ausgeben. Wie Sie sehen können, wurde der ursprüngliche String in Kleinschreibung umgewandelt.

# Tiefere Einblicke
Diese Praxis, Strings in kleinere Buchstaben zu konvertieren, hat eine lange Geschichte und wurde in frühen Programmiersprachen wie BASIC und FORTRAN verwendet. Es hat sich jedoch in modernen Programmiersprachen wie Python etabliert und wird von Programmierern aufgrund seiner Nützlichkeit und Einfachheit geschätzt.

Es gibt auch alternative Möglichkeiten, Strings in Kleinbuchstaben zu konvertieren, wie z.B. mithilfe von regulären Ausdrücken. Dies erfordert jedoch etwas mehr Erfahrung und kann zu unerwünschten Ergebnissen führen, wenn nicht sorgfältig angewendet.

Für diejenigen, die gerne unter der Haube schauen, verwendet die "lower()" Funktion in Python tatsächlich die UNICODE-Zeichentabelle, um Großbuchstaben in Kleinbuchstaben zu konvertieren. Diese Zeichentabelle enthält alle Zeichen, die in verschiedenen Sprachen verwendet werden, und Python verwendet sie, um sicherzustellen, dass die Konvertierung korrekt ist.

# Siehe auch
Weitere Informationen zur "lower()" Funktion und anderen String-Operationen finden Sie in der offiziellen Python-Dokumentation unter https://docs.python.org/3/library/stdtypes.html#string-methods.