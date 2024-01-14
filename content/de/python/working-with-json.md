---
title:                "Python: Arbeiten mit json"
simple_title:         "Arbeiten mit json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich schon einmal mit Programmierung beschäftigt haben, haben Sie wahrscheinlich schon von JSON gehört. Dieses Format wird immer beliebter, da es eine einfache Möglichkeit bietet, Daten zu speichern und zu übertragen. In diesem Blog-Beitrag werden wir uns näher mit JSON in der Python-Programmierung beschäftigen, um zu verstehen, warum es so nützlich ist.

## Wie man

Um mit JSON in Python zu arbeiten, müssen Sie das integrierte Modul "json" importieren. Dann können Sie Daten in das JSON-Format konvertieren und umgekehrt. Schauen wir uns ein Beispiel an:

```Python
import json

# Beispiel-Daten
person = {
	"Name": "Max",
	"Alter": 25,
	"Hobbies": ["Lesen", "Sport", "Reisen"]
}

# Konvertieren in JSON
person_json = json.dumps(person)

print(person_json)
```

Die Ausgabe sollte wie folgt aussehen:

```
{"Name": "Max", "Alter": 25, "Hobbies": ["Lesen", "Sport", "Reisen"]}
```

Um Daten aus JSON zu lesen, können Sie die Funktion "loads()" verwenden. Hier ist ein Beispiel, das denselben Datensatz aus der JSON-Ausgabe liest:

```Python
import json

# JSON-Daten
person_json = '{"Name": "Max", "Alter": 25, "Hobbies": ["Lesen", "Sport", "Reisen"]}'

# Konvertieren in Python-Objekt
person = json.loads(person_json)

# Zugriff auf Daten
print("Name:", person["Name"])
print("Alter:", person["Alter"])
print("Hobbies:", person["Hobbies"])
```

Die Ausgabe sollte folgendermaßen aussehen:

```
Name: Max
Alter: 25
Hobbies: ['Lesen', 'Sport', 'Reisen']
```

Wie Sie sehen können, ist die Arbeit mit JSON in Python sehr einfach und intuitiv.

## Weiterführende Informationen

Es gibt noch viel mehr, was man über die Arbeit mit JSON in Python lernen kann. Zum Beispiel können Sie verschachtelte Strukturen erstellen oder Standardformate wie datetime nutzen. Außerdem können Sie mit der Funktion "dump()" JSON-Daten direkt in eine Datei schreiben und mit "load()" aus einer Datei lesen.

Wenn Sie mehr darüber erfahren möchten, besuchen Sie die offizielle Dokumentation für das "json" Modul in Python:

- [JSON-Dokumentation für Python](https://docs.python.org/3/library/json.html)
- [Einführung in das JSON-Modul in Python](https://realpython.com/python-json/)

## Siehe auch

Weitere interessante Ressourcen für die Arbeit mit JSON in Python:

- [Die JSON-Bibliothek in Python](https://www.freecodecamp.org/news/python-json-library/)
- [JSON in Python erklärt](https://stackabuse.com/reading-and-writing-json-to-a-file-in-python/)
- [Parsing JSON-Daten in Python](https://towardsdatascience.com/parsing-json-data-in-python-b6a2123b19b6)