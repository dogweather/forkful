---
title:    "Python: Die Länge eines Strings bestimmen"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist eine häufige Aufgabe in der Programmierung. Es kann hilfreich sein, um zu überprüfen, ob ein Eingabetext eine bestimmte Mindestlänge hat oder um Teile eines Strings zu extrahieren. Mit Python ist es sehr einfach, die Länge eines Strings zu ermitteln.

# Wie geht's

```Python
name = "Max"
print(len(name))
```

Die Funktion "len()" gibt die Anzahl der Zeichen in einem String zurück. In diesem Beispiel wird die Länge des Strings "Max" ausgegeben, die 3 beträgt. Dieser Wert wird direkt an die print()-Funktion übergeben, um ihn auf dem Bildschirm auszugeben.

Um die Länge eines Strings dynamisch zu ermitteln, kann ein Benutzerinput verwendet werden:

```Python
text = input("Gib einen Text hier ein: ")
print("Die Länge des eingegebenen Textes beträgt " + str(len(text)) + " Zeichen.")
```

Der Benutzer wird aufgefordert, einen Text einzugeben, der dann mit der len()-Funktion analysiert wird. Da die Funktion eine Ganzzahl zurückgibt, muss sie für die Ausgabe zu einem String umgewandelt werden.

# Tiefer Einblick

Die len()-Funktion kann nicht nur auf Strings angewendet werden, sondern auch auf Listen, Tupel und Dictionaries. Sie gibt in diesen Fällen die Anzahl der Elemente zurück. Auch bei Strings berücksichtigt sie alle Zeichen, einschließlich Leerzeichen und Sonderzeichen.

Es ist auch möglich, eine Funktion zu erstellen, die die Länge eines Strings ermittelt:

```Python
def length(text):
    count = 0
    for character in text:
        count += 1
    return count

print(length("Hallo, Welt!")) # Ausgabe: 12
```

In diesem Beispiel wird eine Schleife verwendet, um jeden einzelnen Buchstaben des Strings zu durchlaufen und eine Zählervariable zu erhöhen. Am Ende gibt die Funktion den Wert der Zählervariable zurück, der der Länge des Strings entspricht.

# Siehe auch

- Offizielle Python-Dokumentation zu len(): https://docs.python.org/de/3/library/functions.html#len
- "Python Programmieren für Anfänger" von Jason Cannon: https://www.udemy.com/course/python-programmieren-fuer-anfaenger/