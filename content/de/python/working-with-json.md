---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für JavaScript Object Notation. Es ist ein beliebtes Datenformat zum Austausch von Daten zwischen Server und Webanwendungen. Programmierer verwenden es, weil es leicht lesbar und einfach zu parsen ist.

## How to:
```Python
import json

# JSON-String in Python-Dictionary umwandeln
json_string = '{"name": "Max", "age": 30, "city": "Berlin"}'
python_dict = json.loads(json_string)
print(python_dict)
# Ausgabe: {'name': 'Max', 'age': 30, 'city': 'Berlin'}

# Python-Dictionary in JSON-String umwandeln
dict_to_json = json.dumps(python_dict, indent=4)
print(dict_to_json)
# Ausgabe:
# {
#     "name": "Max",
#     "age": 30,
#     "city": "Berlin"
# }
```

## Deep Dive
JSON wurde Anfang der 2000er Jahre entwickelt und ist seitdem stetig populärer geworden, zum Teil weil es so gut mit JavaScript zusammenarbeitet. Alternativen wie XML existieren zwar noch, sind aber weniger kompakt. In Python werden JSON-Strukturen als Dictionaries und Listen repräsentiert, was die Arbeit mit JSON-Daten sehr intuitiv macht.

## See Also
- Offizielle JSON-Website: https://www.json.org/json-de.html
- Python-Modul `json`: https://docs.python.org/3/library/json.html
- W3Schools JSON Tutorial: https://www.w3schools.com/js/js_json_intro.asp
