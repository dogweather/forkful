---
title:                "Praca z yaml"
html_title:           "PHP: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-yaml.md"
---

{{< edit_this_page >}}

# Praca z YAML w PHP: Informalne podejście do programowania

## Co i Dlaczego?

Praca z YAML w PHP polega na analizie i przetwarzaniu strukturalnych danych zapisanych w formacie YAML. Ten format jest popularny wśród programistów ze względu na czytelność i łatwość w analizie przez komputery. Wykorzystuje się go głównie do przechowywania konfiguracji, ustawień czy danych przechowywanych w plikach.

## Jak to zrobić?

Przykładowy kod w PHP do pracy z YAML może wyglądać następująco:

```PHP
// Wgranie biblioteki
require_once 'symfony/yaml';

// Odczytanie pliku YAML
$data = Symfony\Component\Yaml\Yaml::parse(file_get_contents('example.yaml'));

// Wyświetlenie danych
var_dump($data);
```

Przykładowy plik YAML, z którego zostaną odczytane dane w powyższym kodzie, może wyglądać następująco:

```YAML
# Example YAML file
title: "Working with YAML in PHP"
author: "John Smith"
categories:
  - PHP
  - YAML
  - Programming
```

Wyjście po przetworzeniu takiego pliku będzie następujące:

```PHP
array(3) {
  ["title"]=>
  string(25) "Working with YAML in PHP"
  ["author"]=>
  string(10) "John Smith"
  ["categories"]=>
  array(3) {
    [0]=>
    string(3) "PHP"
    [1]=>
    string(4) "YAML"
    [2]=>
    string(11) "Programming"
  }
}
```

## Wnikliwe podejście

YAML został wprowadzony w 2001 roku i jest skrótem od "YAML Ain't Markup Language". Jego celem było stworzenie formatu, który byłby łatwiejszy w zapisie i odczycie przez człowieka, ale także maszyny. Alternatywą dla formatu YAML są między innymi XML i JSON, jednak YAML wyróżnia się swoją prostotą i czytelnością. W implementacji PHP można wykorzystać bibliotekę symfony/yaml, która oferuje funkcje do analizy i przetwarzania plików YAML.

## Zobacz również

Dla więcej informacji o formacie YAML i sposobach jego wykorzystania, zapoznaj się z dokumentacją:

- [Oficjalna strona YAML](https://yaml.org/)
- [Biblioteka symfony/yaml](https://symfony.com/doc/current/components/yaml.html)
- [Tutorial: Praca z YAML w PHP](https://www.php.net/manual/en/function.yaml-parse.php)