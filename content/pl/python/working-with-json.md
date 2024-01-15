---
title:                "Praca z jsonem"
html_title:           "Python: Praca z jsonem"
simple_title:         "Praca z jsonem"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON jest jednym z najpopularniejszych formatów danych używanych w dzisiejszym świecie programowania. Jest to lekki, przenośny i prosty do zrozumienia sposób kodowania i przechowywania danych. Wykorzystując JSON w swoim kodzie, możesz łatwo komunikować się z różnymi aplikacjami i serwisami sieciowymi.

## Jak używać

W Pythonie obsługa danych w formacie JSON jest bardzo prosta i intuicyjna. Do jej wykorzystania potrzebny jest moduł wbudowany `json`, który dostarcza funkcje do kodowania i dekodowania danych JSON. 

```Python
# Importowanie modułu json
import json

# Zdefiniowanie danych w formacie JSON
my_json = '{"name": "Jan", "age": 25, "hobby": "programowanie"}'

# Dekodowanie danych JSON do formatu Python
my_dict = json.loads(my_json)

# Wyświetlenie danych
print(my_dict)
```

Output:
```Python
{'name': 'Jan', 'age': 25, 'hobby': 'programowanie'}
```

Podobnie, możemy również przekonwertować dane z formatu Python na JSON przy użyciu funkcji `json.dumps()`.

```Python
# Zdefiniowanie słownika
my_dict = {'name': 'Anna', 'age': 30, 'hobby': 'fotografia'}

# Konwersja do formatu JSON
my_json = json.dumps(my_dict)

# Wyświetlenie danych
print(my_json)
```

Output:
```Python
{"name": "Anna", "age": 30, "hobby": "fotografia"}
```

Możemy również pracować z bardziej złożonymi strukturami danych, takimi jak listy i zagnieżdżone słowniki.

```Python
# Zdefiniowanie złożonej struktury danych
my_data = {
  "students": [
    {
      "name": "Marcin",
      "age": 22,
      "courses": ["Informatyka", "Matematyka"]
    },
    {
      "name": "Kasia",
      "age": 20,
      "courses": ["Historia", "Literatura"]
    }
  ]
}

# Konwersja do formatu JSON
my_json = json.dumps(my_data)

# Wyświetlenie danych
print(my_json)
```

Output:
```Python
{"students": [{"name": "Marcin", "age": 22, "courses": ["Informatyka", "Matematyka"]}, {"name": "Kasia", "age": 20, "courses": ["Historia", "Literatura"]}]}
```

## Deep Dive

W Pythonie można również odwoływać się do konkretnych elementów w strukturach danych JSON przy użyciu notacji podobnej do odwoływania się do elementów list i słowników.

```Python
# Przykładowe dane JSON
my_json = '{"name": "Julia", "age": 27, "hobbies": ["programowanie", "czytanie", "joga"]}'

# Dekodowanie do formatu Python
my_dict = json.loads(my_json)

# Wyświetlenie jednego konkretnego hobby
print(my_dict["hobbies"][1])
```

Output:
```Python
czytanie
```

Dodatkowo, można również używać argumentów `indent` i `sort_keys` podczas konwersji danych do formatu JSON w celu uzyskania lepiej sformatowanego i uporządkowanego kodu.

```Python
# Konwersja do formatu JSON z wcięciami i uporządkowaniem kluczy
my_json = json.dumps(my_dict, indent=4, sort_keys=True)

# Wyświetlenie danych
print(my_json)
```

Output:
```Python
{
    "age": 27,
    "hobbies": [
        "programowanie",
        "czytanie",
        "joga"
    ],
    "name": "Julia"
}
```

Teraz, gdy wiesz jak łatwo obsługiwać JSON w Pythonie, możesz bez problemu integrować się z innymi aplikacjami i serwisami i wykorzystywać dane w formacie JSON w swo