---
title:                "Praca z yaml"
html_title:           "Swift: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Planujemy zacząć pracę z YAML w Swift? W takim razie dobrze trafiłeś! Ten język znaczników jest często wykorzystywany w aplikacjach i narzędziach do przechowywania danych w formacie tekstowym. Dzięki swojej czytelnej strukturze i prostocie użycia, YAML jest idealnym wyborem dla programistów.

## Jak to zrobić

### Instalacja

Aby rozpocząć pracę z YAML w Swift, musisz najpierw zainstalować bibliotekę YAMLSwift. Możesz to zrobić przy użyciu menedżera zależności, takiego jak CocoaPods lub Carthage, lub ręcznie pobrać i dodać ją do swojego projektu.

### Tworzenie pliku YAML

Aby utworzyć plik YAML, możesz użyć następującej struktury:

```Swift
let yaml = """
animal: dog
name: Buddy
age: 5
breed: Labrador
"""
```

Warto zauważyć, że klucze i wartości są oddzielone dwukropkiem, a pary klucz-wartość są oddzielone przecinkiem.

### Parsowanie pliku YAML

Aby przetworzyć plik YAML na obiekty w Swift, możesz użyć klasy YamlParser. Na przykład, jeśli chcesz sparsować powyższy przykład, możesz użyć następującego kodu:

```Swift
let parser = YamlParser()
let yamlObject = try parser.parse(yaml)
```

### Odczytywanie danych z obiektu YAML

Po sparsowaniu pliku YAML na obiekt, możesz odczytać jego wartości w następujący sposób:

```Swift
let animal = yamlObject["animal"]
print(animal) // Output: dog

let name = yamlObject["name"]
print(name) // Output: Buddy

let age = yamlObject["age"]
print(age) // Output: 5

let breed = yamlObject["breed"]
print(breed) // Output: Labrador
```

W przypadku gdy klucz nie istnieje, wartość zwracana będzie równa nil.

## Deep Dive

Ponieważ YAML obsługuje różne typy danych, możesz również przechowywać w nim tablice i obiekty w formacie JSON. Możesz również tworzyć zagnieżdżone struktury, co daje większą elastyczność w przechowywaniu danych.

Ponadto, narzędzie Jazzy oferuje możliwość generowania dokumentacji z plików YAML, co znacznie ułatwia zrozumienie struktury danych i umożliwia szybsze wprowadzenie zmian.

## Zobacz także

- [Dokumentacja YAMLSwift](https://github.com/behrang/YAMLSwift)
- [Narzędzie Jazzy](https://github.com/realm/jazzy)
- [Oficjalna strona YAML](https://yaml.org/)