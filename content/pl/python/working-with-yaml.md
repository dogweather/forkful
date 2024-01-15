---
title:                "Praca z plikiem yaml"
html_title:           "Python: Praca z plikiem yaml"
simple_title:         "Praca z plikiem yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-yaml.md"
---

{{< edit_this_page >}}

# Dlaczego

Dlaczego zdecydowałbyś się na pracę z YAML? Otóż jest to bardzo popularny format danych, który jest wykorzystywany w wielu aplikacjach i narzędziach programistycznych. Praca z YAML może być niezwykle przydatna w zarządzaniu konfiguracją, przechowywaniu danych czy integracji pomiędzy różnymi systemami.

# Jak to zrobić

Aby rozpocząć pracę z YAML w Pythonie, musimy najpierw zainstalować odpowiedni moduł za pomocą polecenia `pip install pyyaml`. Następnie możemy już bez problemu importować go do swojego kodu:

```python
import yaml
```

## Wczytywanie i zapisywanie danych

Aby wczytać dane z pliku YAML, możemy skorzystać z metody `load`:

```python
with open('dane.yaml', 'r') as f:
  data = yaml.load(f, Loader=yaml.FullLoader)
```

Wynikiem tej operacji będzie obiekt Pythona zawierający wczytane dane. Aby zapisać dane do pliku YAML, możemy wykorzystać metodę `dump`:

```python
data = {
  'imie': 'Anna',
  'nazwisko': 'Kowalska',
  'wiek': 30
}

with open('dane.yaml', 'w') as f:
  yaml.dump(data, f)
```

Ten prosty przykład pokazuje, że praca z YAML w Pythonie jest bardzo prosta i intuicyjna.

## Przetwarzanie danych

Praca z danymi w formacie YAML w Pythonie jest możliwa dzięki wykorzystaniu dwóch typów obiektów: `dict` i `list`. Możemy na nich wykonywać różnego rodzaju operacje, takie jak iterowanie, dodawanie lub usuwanie elementów.

```python
import yaml

with open('dane.yaml', 'r') as f:
  data = yaml.load(f, Loader=yaml.FullLoader)

# Iterowanie po słowniku
for key, value in data.items():
  print(f'{key}: {value}')

# Dodawanie nowego klucza i wartości
data['miasto'] = 'Warszawa'

# Usuwanie klucza i wartości
del data['wiek']

# Zapisywanie danych do pliku YAML
with open('nowe_dane.yaml', 'w') as f:
  yaml.dump(data, f)
```

## Deep Dive

W formacie YAML możemy wykorzystywać również bardziej złożone struktury danych, takie jak listy zagnieżdżone czy słowniki z listami jako wartości. Możemy również definiować własne typy danych i wykorzystywać je w plikach YAML.

```yaml
# Przykład listy zagnieżdżonej
- Anna
- Kowalska
- adresy:
  - Warszawa
  - Gdańsk

# Przykład definiowania własnego typu danych
!!osoba
imie: Anna
nazwisko: Kowalska
wiek: 30
```

Dzięki temu, praca z YAML w Pythonie jest jeszcze bardziej elastyczna i możemy dostosować ją do swoich potrzeb.

# Zobacz również

- Oficjalna dokumentacja modułu YAML dla Pythona: https://pyyaml.org/wiki/PyYAMLDocumentation
- Przydatne narzędzia i biblioteki Python do pracy z YAML: https://realpython.com/python-yaml/
- Poradnik z praktycznymi przykładami wykorzystania YAML w Pythonie: https://towardsdatascience.com/an-introduction-to-yaml-2bcb433afd8e