---
title:                "Praca z json"
html_title:           "Python: Praca z json"
simple_title:         "Praca z json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON (JavaScript Object Notation) to popularny format danych używany przez programistów do przechowywania i wymiany informacji. W przeciwieństwie do innych formatów, takich jak XML czy CSV, JSON jest prostszy w użyciu i ma bardziej przejrzystą składnię. Programiści stosują go do przechowywania i przesyłania danych między aplikacjami, serwerami oraz bazami danych.

## Jak to zrobić:
Aby pracować z JSON w Pythonie, możemy skorzystać z modułu wbudowanego o nazwie "json". Najpierw musimy zaimportować ten moduł, a następnie można używać jego funkcji do odczytu, zapisu oraz manipulacji danymi JSON. Poniżej znajdują się przykładowe kody wraz z wynikami:

```Python
import json

# Przykładowy obiekt JSON:
person = '{"name": "Anna", "age": 30, "city": "Warsaw"}'

# Odczytanie JSON i przekształcenie go w słownik:
person_dict = json.loads(person)
print(person_dict)
# Output: {'name': 'Anna', 'age': 30, 'city': 'Warsaw'}

# Dodanie nowego klucza i wartości do słownika:
person_dict["job"] = "Programmer"
print(person_dict)
# Output: {'name': 'Anna', 'age': 30, 'city': 'Warsaw', 'job': 'Programmer'}

# Konwersja słownika na JSON:
person_json = json.dumps(person_dict)
print(person_json)
# Output: {"name": "Anna", "age": 30, "city": "Warsaw", "job": "Programmer"}

```

## W głąb:
JSON został stworzony jako alternatywa dla formatu XML w 2001 roku przez Douglasa Crockforda, który jest również jednym z autorów języka JavaScript. Od tego czasu JSON stał się bardzo popularnym formatem w świecie programowania, a jego prostota i czytelność przyczyniły się do jego rozpowszechnienia. Alternatywnymi formatami do przechowywania i wymiany danych w Pythonie są m.in. XML, YAML oraz protocol buffers. Ponadto, warto pamiętać, że w Pythonie możemy również wykorzystać wbudowane funkcje do obsługi formatu CSV (comma-separated values).

Jeśli chcesz dowiedzieć się więcej o pracy z JSON w Pythonie, możesz zgłębić dokumentację modułu "json": https://docs.python.org/3/library/json.html#module-json

## Zobacz także:
- Oficjalna strona formatu JSON: https://json.org/
- Przykładowa strona zawierająca dane w formacie JSON: https://jsonplaceholder.typicode.com/
- Porównanie formatów JSON i XML: https://www.jsonxml.com/