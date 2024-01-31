---
title:                "Praca z yaml"
date:                  2024-01-19
simple_title:         "Praca z yaml"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML to ludzki język danych, popularny w konfiguracjach i plikach projektów. Programiści używają go ze względu na jego czytelność i prostotę obsługi przez różne języki programowania, w tym PHP.

## Jak to zrobić:
Poniżej znajdziesz przykłady, jak obsłużyć YAML w PHP. Najpierw musisz zainstalować rozszerzenie yaml dla PHP, używając `pecl install yaml` lub `composer require symfony/yaml`.

**Odczyt YAML:**
```PHP
<?php
require 'vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

$yamlContent = <<<YAML
- just: another YAML
- example:
    - with: multiple
      values
YAML;

$data = Yaml::parse($yamlContent);
print_r($data);
```

**Output:**
```
Array
(
    [0] => Array
        (
            [just] => another YAML
        )
    [1] => Array
        (
            [example] => Array
                (
                    [0] => Array
                        (
                            [with] => multiple
                            [values] => 
                        )
                )
        )
)
```

**Zapis do YAML:**
```PHP
<?php
require 'vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

$array = [
    'foo' => 'bar',
    'bar' => ['baz', 'qux']
];

$yaml = Yaml::dump($array);
echo $yaml;
```

**Output:**
```
foo: bar
bar: [baz, qux]
```

## Deep Dive
YAML, który oznacza "YAML Ain't Markup Language", pojawił się w 2001 roku jako alternatywa dla XML. Pomimo wolniejszej wydajności niż JSON oraz ograniczonej obsługi binarnych danych, jego przejrzystość ułatwia zarządzanie konfiguracją. W PHP YAML można obsługiwać zarówno przez native PECL extension, jak i biblioteki, takie jak Symfony YAML. Zawsze warto sprawdzać kompatybilność wersji bibliotek z PHP i być świadomym potencjalnych problemów z bezpieczeństwem podczas ładowania niezaufanego YAML.

## Zobacz też:
- [Symfony YAML Component Documentation](https://symfony.com/doc/current/components/yaml.html)
- [The Official YAML Website](https://yaml.org)
