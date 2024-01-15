---
title:                "Praca z yaml"
html_title:           "Ruby: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś powinien zainteresować się pracą z YAML? Ponieważ jest to wygodny i intuicyjny format do przechowywania danych, szczególnie przydatny w programowaniu.

## Jak to zrobić

Najpierw musimy zainstalować bibliotekę yaml, używając jednego z menadżerów pakietów, takich jak Bundler lub Gem:
```Ruby
gem install yaml
```

Teraz możemy zacząć używać YAML w naszym kodzie. Przykładowo, możemy stworzyć plik YAML zawierający informacje o naszej firmie:
```Ruby
company = {
  name: "MojaFirma",
  employees: [
    {
      name: "Jan Kowalski",
      position: "Inżynier",
      salary: 5000
    },
    {
      name: "Anna Nowak",
      position: "Specjalista ds. marketingu",
      salary: 4000
    },
    {
      name: "Piotr Nowakowski",
      position: "Księgowy",
      salary: 4500
    }
  ]
}
```

Możemy zapisać ten obiekt do pliku YAML przy użyciu metody `to_yaml`:
```Ruby
File.open('company.yaml', 'w') do |file|
  file.write(company.to_yaml)
end
```

Następnie możemy odczytać zawartość pliku YAML i wyświetlić wynik na ekranie:
```Ruby
yaml_data = File.read('company.yaml')
puts yaml_data
```

Kod ten wyświetli następujący wynik:
```
---
:name: MojaFirma
:employees:
- :name: Jan Kowalski
  :position: Inżynier
  :salary: 5000
- :name: Anna Nowak
  :position: Specjalista ds. marketingu
  :salary: 4000
- :name: Piotr Nowakowski
  :position: Księgowy
  :salary: 4500
```

## Głębsza analiza

Format YAML jest oparty na języku przechowywania danych YAML (YAML Ain't Markup Language) i jest używany głównie do przechowywania konfiguracji lub danych. Jego struktura jest oparta na nadrzędnych kluczach i ich wartościach, które mogą być tablicami lub obiektami.

Ważne jest również, aby zawsze przestrzegać składni YAML, ponieważ jest ona bardzo wrażliwa na wcięcia i używanie tabulatorów zamiast spacji może powodować błędy w działaniu kodu.

Podczas pracy z YAML, warto również zapoznać się z różnymi bibliotekami dostępnymi dla języka Ruby, takimi jak `psych` lub `YAML.rb`, które oferują dodatkowe funkcje, takie jak walidacja danych lub konwersja pomiędzy formatami.

## Zobacz także

- [Dokumentacja YAML w języku Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/yaml/rdoc/YAML.html)
- [Kurs YAML na stronie Learn Ruby the Hard Way](https://learnrubythehardway.org/book/ex51.html)
- [Poradnik na temat pracy z YAML w Ruby on Rails](https://pragmaticstudio.com/tutorials/working-with-yaml)