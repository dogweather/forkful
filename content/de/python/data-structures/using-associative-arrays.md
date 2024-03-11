---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:39.460256-07:00
description: "Assoziative Arrays, in Python als Dictionaries bekannt, ordnen Schl\xFC\
  ssel Werten zu, was das Abrufen, Modifizieren oder Nachverfolgen von Daten anhand\u2026"
lastmod: '2024-03-11T00:14:27.334237-06:00'
model: gpt-4-0125-preview
summary: "Assoziative Arrays, in Python als Dictionaries bekannt, ordnen Schl\xFC\
  ssel Werten zu, was das Abrufen, Modifizieren oder Nachverfolgen von Daten anhand\u2026"
title: Verwendung von assoziativen Arrays
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays, in Python als Dictionaries bekannt, ordnen Schlüssel Werten zu, was das Abrufen, Modifizieren oder Nachverfolgen von Daten anhand einer eindeutigen Kennung erleichtert. Programmierer nutzen sie aufgrund ihrer Effizienz beim Zugriff auf Elemente und ihrer Flexibilität bei der Darstellung komplexer Datenstrukturen.

## Wie geht das:

Ein Dictionary in Python zu erstellen, ist unkompliziert. Man schließt Schlüssel-Wert-Paare in geschweifte Klammern `{}`, wobei Schlüssel und Werte durch einen Doppelpunkt getrennt sind:

```Python
# Ein assoziatives Array (Dictionary) erstellen
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

Ausgabe:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

Auf einen Wert über seinen Schlüssel zuzugreifen, ist einfach:

```Python
# Auf einen Wert zugreifen
print(my_dict["name"])
```

Ausgabe:
```
John
```

Das Hinzufügen oder Aktualisieren von Elementen erfolgt durch Zuweisen eines Werts zu einem Schlüssel:

```Python
# Ein neues Schlüssel-Wert-Paar hinzufügen
my_dict["email"] = "john@example.com"
# Einen Wert aktualisieren
my_dict["age"] = 31
print(my_dict)
```

Ausgabe:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

Um über die Elemente des Dictionaries zu iterieren:

```Python
# Durch Schlüssel-Wert-Paare iterieren
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Ausgabe:
```
name: John
age: 31
city: New York
email: john@example.com
```

## Tiefergehend

Assoziative Arrays in Python, oder Dictionaries, wurden eingeführt, um eine Datenstruktur für effizienten Datenzugriff und -manipulation zu bieten. Anders als Sequenzen, die durch eine Reihe von Zahlen indiziert sind, werden Dictionaries durch Schlüssel indiziert, die jeden unveränderlichen Typ annehmen können. Diese Designentscheidung macht Dictionaries ideal geeignet für schnelle Nachschlagetabellen, in denen Schlüssel eindeutige Werte zugeordnet sind.

Historisch gesehen wurden Python-Dictionaries mithilfe einer Hash-Tabelle implementiert, was sicherstellt, dass die durchschnittliche Zeitkomplexität für Such-, Einfüge- und Löschoperationen O(1) ist. Ab Python 3.6 behalten Dictionaries auch die Einfügereihenfolge der Elemente bei, was die Vorteile von Hash-Tabellen mit der Vorhersehbarkeit der Einfügereihenfolge, wie bei geordneten Datenstrukturen zu sehen, verbindet.

Obwohl Dictionaries unglaublich vielseitig sind, könnten in einigen spezialisierten Fällen Alternativen wie `collections.defaultdict` oder `collections.OrderedDict` (vor Python 3.7) vorzuziehen sein. `defaultdict` ist besonders nützlich, wenn ein Dictionary für nicht vorhandene Schlüssel einen Standardwert zurückgeben soll, was bestimmte Arten von bedingter Logik vereinfacht. Jedoch bleibt die eingebaute Dictionary-Klasse mit der kontinuierlichen Verbesserung und Evolution von Python oft die erste Wahl für assoziative Arrays wegen ihrer Robustheit und der Bequemlichkeit, die sie direkt aus der Box bietet.
