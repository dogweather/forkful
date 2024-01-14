---
title:                "Python: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft Situationen, in denen wir einen String in Großbuchstaben umwandeln müssen. Dies kann aus verschiedenen Gründen geschehen, wie z.B. um einen String in einem bestimmten Format anzuzeigen oder um Vergleiche zwischen Strings durchzuführen. In dieser Blog-Post werden wir uns genauer anschauen, wie wir Strings in Großbuchstaben umwandeln können und warum dies nützlich sein kann.

## Wie man Strings in Großbuchstaben umwandelt

In Python gibt es eine vordefinierte Methode, um Strings in Großbuchstaben umzuwandeln. Diese Methode heißt `.upper()` und kann auf jeden String angewendet werden. Schauen wir uns ein Beispiel an:

```Python
name = "anne"
print(name.upper())
```

Die Ausgabe dieses Codes wäre "ANNE". Wie wir sehen können, wurden alle Buchstaben im String "name" in Großbuchstaben umgewandelt. Dies kann besonders nützlich sein, wenn wir beispielsweise sicherstellen wollen, dass alle Nutzernamen in einer Datenbank in Großbuchstaben gespeichert werden.

## Tiefere Einblicke

Es ist wichtig zu beachten, dass die `.upper()` Methode lediglich eine Kopie des ursprünglichen Strings erstellt und diese in Großbuchstaben zurückgibt. Der ursprüngliche String bleibt unverändert. Dies ist aufgrund der Eigenschaft von Strings in Python, unveränderlich zu sein. Dies bedeutet, dass sie nicht direkt bearbeitet werden können, sondern dass für Bearbeitungen immer eine neue Kopie erstellt werden muss.

Zudem gibt es Alternativen zur `.upper()` Methode, wie z.B. die `.capitalize()` Methode, die nur den ersten Buchstaben eines Strings in Großbuchstaben umwandelt, oder die `.title()` Methode, die jeden ersten Buchstaben eines Wortes in einem String in Großbuchstaben umwandelt. Es ist wichtig, die verschiedenen Methoden zu kennen und je nach Anwendungszweck die passende auszuwählen.

## Siehe auch

Weitere Informationen über die Verwendung von Strings in Python können Sie in der offiziellen Python-Dokumentation finden: 
- [Strings in der Python-Dokumentation](https://docs.python.org/de/3/library/stdtypes.html#textseq)
- [Python String Methoden](https://www.w3schools.com/python/python_ref_string.asp)
- [Tutorial zu String Operations in Python](https://realpython.com/python-strings/)