---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist ein Datenformat zum Speichern und Übertragen einfacher bis komplexer Datenstrukturen. Programmierer nutzen YAML wegen seiner einfachen Les- und Schreibbarkeit, um Konfigurationen und Daten zwischen verschiedenen Sprachen und Diensten auszutauschen.

## How to:
### YAML-Datei laden:
```php
<?php
// symfony/yaml Komponente installieren via Composer
require_once 'vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAML-Datei als Array einlesen
$yaml = Yaml::parseFile('config.yaml');

print_r($yaml);
```

### YAML-String in PHP Array umwandeln:
```php
<?php
$yamlStr = <<<YAML
user: admin
password: secret
roles:
  - admin
  - user
YAML;

$array = Yaml::parse($yamlStr);

print_r($array);
```

### PHP Array in YAML umwandeln:
```php
<?php
$array = [
    'user' => 'admin',
    'password' => 'secret',
    'roles' => ['admin', 'user'],
];

$yaml = Yaml::dump($array);

echo $yaml;
```

## Deep Dive
YAML, "YAML Ain't Markup Language", ist ein rekursives Akronym und steht für Daten, die menschenlesbar und maschinenverarbeitbar sind. YAML entstand Anfang der 2000er als Alternative zu XML und anderen schwer lesbaren Datenformaten. Es wird oft für Konfigurationsdateien eingesetzt. In der PHP-Welt können die `symfony/yaml`-Bibliothek oder die `spyc`-Bibliothek verwendet werden, aber die Symfony-Komponente ist beliebter und wird aktiv entwickelt.

## See Also
- Die offizielle YAML-Website: [yaml.org](https://yaml.org)
- Symfony YAML Komponente: [Symfony Yaml Component](https://symfony.com/doc/current/components/yaml.html)
- YAML Syntax Erklärung: [Learn YAML in Y Minutes](https://learnxinyminutes.com/docs/yaml/)