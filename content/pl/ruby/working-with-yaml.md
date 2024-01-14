---
title:                "Ruby: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

YAML (Yet Another Markup Language) jest popularnym formatem do przechowywania i przesyłania danych w aplikacjach internetowych oraz konfiguracji programistycznych. Jest to łatwy w użyciu format plików tekstowych, który jest przyjazny dla ludzi i maszyn. Praca z YAML może pomóc programistom w ułatwieniu przetwarzania, przenoszenia i przechowywania danych w aplikacjach.

## Jak to zrobić

Aby zacząć pracować z YAML w języku Ruby, musisz najpierw zainstalować bibliotekę YAML bibliotekę. Możesz to zrobić poprzez wykorzystanie menedżera pakietów Ruby, na przykład RubyGems, lub instalując pakiet yaml z Twojego menedżera pakietów systemu operacyjnego.

Po zainstalowaniu biblioteki możesz rozpocząć tworzenie, przetwarzanie i odczyt danych w formacie YAML w swojej aplikacji Ruby.

```Ruby
require 'yaml'

# Tworzenie prostego pliku YAML
data = {
  name: 'Janek',
  age: 30,
  occupation: 'Programista'
}
File.write('dane.yml', data.to_yaml)

# Odczyt danych z pliku YAML
daty = YAML.load(File.read('dane.yml'))
puts daty[:name] # output: Janek

# Przetwarzanie danych YAML do Ruby
yaml_data = "
  marki samochodowe:
    - Toyota
    - BMW
    - Mercedes-Benz
"
car_brands = YAML.load(yaml_data)
puts car_brands['marks of cars'] # output: ["Toyota", "BMW", "Mercedes-Benz"]
```

## Wnikliwa analiza

YAML jest łatwym formatem do czytania i edycji przez ludzi, ponieważ jest podobny do języka angielskiego i posiada wyraźną strukturę. Jednak praca z YAML może być trudna dla maszyn, ponieważ niektóre typy danych, takie jak daty i tablice, muszą być przekonwertowane na odpowiednie typy danych Ruby.

Istnieje również wiele zaawansowanych funkcji YAML, które pozwalają na tworzenie bardziej złożonych struktur danych, takich jak mapy i pętle, które mogą ułatwić tworzenie i przetwarzanie danych w aplikacjach.

Podczas pracy z YAML warto również pamiętać o bezpieczeństwie. Pliki YAML mogą zawierać kod, który może być wykonany przez aplikację, dlatego ważne jest, aby upewnić się, że nie zawierają one niepożądanych poleceń lub danych.

## Zobacz również

- Dokumentacja YAML: https://yaml.org/
- RubyGems: https://rubygems.org/
- Przykłady z GitHub: https://github.com/alexdlaird/yaml-sample