---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per serializzare dati - pensa a un JSON ma più leggibile dall'uomo. I programmatori lo usano per configurazioni o dati strutturati semplici, perché è chiaro e facile da scrivere e leggere.

## Come fare:
PHP gestisce YAML con l'estensione 'yaml'; se non installata, usa `pecl install yaml`.

```PHP
<?php
$yaml = <<<EOD
primo: Primo elemento
secondo:
  - Lista
  - Di
  - Elementi
EOD;

// Converti YAML in PHP Array
$array = yaml_parse($yaml);
print_r($array);

// Output
Array
(
    [primo] => Primo elemento
    [secondo] => Array
        (
            [0] => Lista
            [1] => Di
            [2] => Elementi
        )

)

// Converti Array PHP in YAML
$array = [
    'primo' => 'Primo elemento',
    'secondo' => ['Lista', 'Di', 'Elementi'],
];

$yaml = yaml_emit($array);
echo $yaml;

// Output
primo: Primo elemento
secondo:
  - Lista
  - Di
  - Elementi

?>
```

## Approfondimenti
Nato nei primi anni 2000, YAML stava per "Yet Another Markup Language", ma ora è "YAML Ain't Markup Language". Alternativa a XML e JSON, YAML è ottimo per la leggibilità umana ma attenzione a errori di indentazione. In PHP, usa `yaml_parse` per interpretare e `yaml_emit` per generare YAML; queste funzioni gestiscono rispettivamente la decodifica e la codifica dei tuoi dati.

## Vedi Anche:
- Documentazione ufficiale YAML: https://yaml.org
- Estensione PHP YAML su PECL: https://pecl.php.net/package/yaml
- Documentazione PHP per funzioni YAML: https://www.php.net/manual/en/book.yaml.php
