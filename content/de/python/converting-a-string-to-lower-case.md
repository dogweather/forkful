---
title:                "Ein String in Kleinbuchstaben umwandeln"
html_title:           "Python: Ein String in Kleinbuchstaben umwandeln"
simple_title:         "Ein String in Kleinbuchstaben umwandeln"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Manchmal ist es notwendig, einen Text in Kleinbuchstaben umzuwandeln, um beispielsweise Vergleiche durchzuführen oder eine einheitliche Formatierung zu erhalten. In Python ist dies mit einer einfachen Funktion möglich, die wir in diesem Artikel genauer betrachten werden.

## Wie geht's

Die Funktion zum Umwandeln eines Strings in Kleinbuchstaben lautet ```lower()```. Sie wird einfach auf den entsprechenden String angewendet und gibt den Text in Kleinbuchstaben zurück.

```Python
text = "Hallo Welt!"
print(text.lower())
```

Das oben genannte Beispiel gibt folgende Ausgabe:

```Python
hallo welt!
```

Wie Sie sehen können, wurden alle Großbuchstaben in Kleinbuchstaben umgewandelt.

Es ist auch möglich, die Funktion direkt auf einer Eingabe des Benutzers anzuwenden, indem man ```input()``` und ```lower()``` kombiniert:

```Python
text = input("Geben Sie einen beliebigen Text ein: ")
print(text.lower())
```

Jetzt können Sie jeden beliebigen Text eingeben und das Ergebnis in Kleinbuchstaben erhalten.

## Tiefergehende Einblicke

Für diejenigen, die sich für die Funktionsweise der ```lower()``` Funktion interessieren, hier ein kurzer Einblick:

- Die Funktion wandelt nicht nur Buchstaben, sondern auch Zahlen und Sonderzeichen in kleinere Buchstaben um.
- Sie funktioniert auch mit Sonderzeichen aus anderen Sprachen wie z.B. ä, ö, ü.
- Um zu überprüfen, ob ein String bereits in Kleinbuchstaben vorliegt, können Sie die Funktion ```islower()``` verwenden.

## Siehe auch

- [Python Dokumentation zur lower()-Funktion](https://docs.python.org/de/3/library/stdtypes.html#str.lower)
- [Weitere nützliche String-Funktionen in Python](https://www.geeksforgeeks.org/python-string-functions/)
- [Eine Liste von 10 Python-Befehlen, die jeder wissen sollte](https://www.edureka.co/blog/basic-python-commands-for-beginners/)