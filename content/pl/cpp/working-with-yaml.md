---
title:                "Praca z plikiem yaml"
html_title:           "C++: Praca z plikiem yaml"
simple_title:         "Praca z plikiem yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

# Co i po co?
Praca z YAML-em to popularna metoda w świecie programowania, polegająca na tworzeniu plików konfiguracyjnych w formacie YAML. Służą one do przechowywania ustawień i danych, wykorzystywanych przez programy w czasie pracy. Programiści wybierają pracę z YAML-em ze względu na czytelność i prostotę tego języka, co ułatwia zarządzanie ustawieniami i ich edycję.

# Jak to zrobić?
Aby pracować z YAML-em w języku C++, należy najpierw zainstalować odpowiednią bibliotekę. Najpopularniejszą jest biblioteka YAML-CPP, która jest dostępna na wielu platformach i jest prostsza w użyciu niż inne rozwiązania. Poniżej znajdują się przykładowe kody wraz z wyjściami, które ilustrują podstawowe operacje na plikach YAML.

## Przykładowe plik YAML
```C++
#include <yaml-cpp/yaml.h>

int main() {
  // Tworzenie pliku YAML
  YAML::Emitter out;
  out << YAML::BeginMap;
  out << YAML::Key << "imie";
  out << YAML::Value << "Jan";
  out << YAML::Key << "nazwisko";
  out << YAML::Value << "Kowalski";
  out << YAML::Key << "wiek";
  out << YAML::Value << 35;
  out << YAML::EndMap;

  // Zapis do pliku
  std::ofstream fout("dane.yaml");
  fout << out.c_str();
  fout.close();

  // Odczyt i wyświetlenie danych z pliku
  YAML::Node dane = YAML::LoadFile("dane.yaml");
  std::cout << "Imię: " << dane["imie"].as<std::string>() << std::endl;
  std::cout << "Nazwisko: " << dane["nazwisko"].as<std::string>() << std::endl;
  std::cout << "Wiek: " << dane["wiek"].as<int>() << std::endl;
  
  return 0;
}
```

## Wyjście
```
Imię: Jan
Nazwisko: Kowalski
Wiek: 35
```

# Głębsze zanurzenie
YAML został opracowany w 2001 roku przez Clarka Evansa i Ingy'ego döt Net jako alternatywa dla języków XML i JSON. W odróżnieniu od tych dwóch, YAML jest znacznie prostszy i czytelniejszy dzięki zastosowaniu wyraźnej struktury opartej na wcięciach. Istnieją również inne biblioteki do pracy z YAML-em w C++, takie jak: yaml-cpp, libyaml czy QtYaml.

# Zobacz także
Jeśli chcesz poznać więcej o YAML-u, polecamy następujące źródła:
- Oficjalna dokumentacja YAML-a: https://yaml.org/
- Biblioteka YAML-CPP: https://github.com/jbeder/yaml-cpp
- Porównanie YAML-a z innymi formatami: https://yaml.com/