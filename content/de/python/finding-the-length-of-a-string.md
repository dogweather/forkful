---
title:                "Python: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings kann für Python-Programmiererinnen und -Programmierer ein wichtiges Werkzeug sein, um die Länge von Wörtern oder Sätzen in einem Text zu bestimmen. Dies kann hilfreich sein, wenn man zum Beispiel die Eingabe von Benutzerinnen und Benutzern überprüfen oder die Ausgabe von Funktionen analysieren möchte.

## Wie geht man vor

Die Länge eines Strings in Python kann mithilfe der eingebauten Funktion `len()` bestimmt werden. Schauen wir uns ein Beispiel an:

```Python
text = "Hallo, Welt!"
print(len(text))
```

Die Ausgabe dieses Codes ist `12`, da der String "Hallo, Welt!" aus 12 Zeichen besteht, einschließlich Leerzeichen und Satzzeichen.

Man kann auch die Länge von Strings in Kombination mit anderen Funktionen verwenden, um bestimmte Teile davon zu extrahieren. Hier ein Beispiel:

```Python
text = "Ich liebe Python!"
print(text[0:10])
print(len(text[0:10]))
```

Die Ausgabe zeigt den Teil des Strings aus den ersten zehn Zeichen (`"Ich liebe"`) sowie dessen Länge (`10`).

## Tiefere Einblicke

Beim Finden der Länge eines Strings ist es wichtig zu beachten, dass Leerzeichen und Satzzeichen auch als Zeichen gezählt werden. Außerdem kann die Länge eines Strings je nach verwendetem Zeichensatz variieren.

Wenn man eine Liste von Strings hat, kann man `len()` auch auf diese anwenden, um die Länge jedes einzelnen Strings in der Liste zu bestimmen. Hier ein Beispiel:

```Python
liste = ['Cat', 'Dog', 'Elephant', 'Giraffe']
for s in liste:
    print(len(s))
```
Die Ausgabe wäre:

```Python
3
3
8
7
```

## Siehe auch

- [Python Built-in Functions](https://docs.python.org/3/library/functions.html#len)
- [w3schools.com: Python String Length](https://www.w3schools.com/python/ref_func_len.asp)
- [Real Python: String Handling in Python](https://realpython.com/python-strings/)