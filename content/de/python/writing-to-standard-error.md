---
title:                "Python: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum
Die Ausgabe von Fehlermeldungen ist ein wesentlicher Bestandteil der Programmierung. Sie helfen dabei, Bugs und Probleme im Code zu identifizieren und zu beheben. Wenn du ein Programmierer bist, ist die Fähigkeit, effektiv an die Standardfehlerausgabe zu schreiben, unerlässlich.

# Wie man es macht
Die Verwendung der Standardfehlerausgabe in Python ist relativ einfach. Alles, was du tun musst, ist, die `stderr`-Funktion aus dem `sys`-Modul zu importieren und dann die `write()`-Methode zu verwenden, um die gewünschte Fehlermeldung auszugeben.

```Python
import sys
sys.stderr.write("Dies ist eine Fehlermeldung.")
```

Dies wird die Meldung "Dies ist eine Fehlermeldung" in roter Schrift auf deinem Terminal oder in der Ausgabekonsole ausgeben. Du kannst auch Variablen in einer Fehlermeldung verwenden, indem du sie in die `write()`-Methode einfügst.

```Python
import sys
zahl = 10

sys.stderr.write("Die Zahl ist: " + str(zahl))
```

Die Ausgabe davon wäre "Die Zahl ist: 10".

# Tiefere Einblicke
Wenn du tiefer in die Standardfehlerausgabe eintauchst, wirst du feststellen, dass es auch andere nützliche Methoden gibt, wie zum Beispiel `flush()`, die alle ausstehenden Daten aus dem Puffer zurückgibt.

```Python
import sys
zahl = 5

sys.stderr.write("Die Zahl ist: " + str(zahl))
sys.stderr.flush()
zahl = 10
```

Im obigen Beispiel wird die ursprüngliche Zahl 5 ausgegeben und dann der Puffer geleert, bevor die Zahl auf 10 aktualisiert wird und somit für die nächste Ausgabe bereit ist.

# Siehe auch
- [Python Standard Library: sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr)
- [Python Standardbibliothek: Die stderr Ausgabe](https://docs.python.org/de/3/tutorial/errors.html#the-standard-error-device)