---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:06.467235-07:00
description: "Wie geht das: PHP unterst\xFCtzt in seinen aktuellen Iterationen das\
  \ Parsen von YAML nicht als Teil seiner Standardbibliothek. Die einfachste M\xF6\
  glichkeit,\u2026"
lastmod: '2024-03-13T22:44:53.992950-06:00'
model: gpt-4-0125-preview
summary: "PHP unterst\xFCtzt in seinen aktuellen Iterationen das Parsen von YAML nicht\
  \ als Teil seiner Standardbibliothek."
title: Arbeiten mit YAML
weight: 41
---

## Wie geht das:
PHP unterstützt in seinen aktuellen Iterationen das Parsen von YAML nicht als Teil seiner Standardbibliothek. Die einfachste Möglichkeit, mit YAML in PHP zu arbeiten, besteht darin, die Symfony YAML-Komponente oder die `yaml` PECL-Erweiterung zu verwenden.

### Verwendung der Symfony YAML-Komponente
Zuerst installieren Sie die Symfony YAML-Komponente über Composer:

```bash
composer require symfony/yaml
```

Dann können Sie YAML-Inhalte wie folgt parsen und ausgeben:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAML parsen
$yamlString = <<<YAML
greet: Hallo, Welt!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Erstellen von YAML aus einem Array
$array = [
    'greet' => 'Hallo, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Beispielausgabe beim Parsen:

```
Array
(
    [greet] => Hallo, Welt!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Beispielausgabe beim Ausgeben:

```
greet: Hallo, YAML!
framework:
    name: Symfony
    language: PHP
```

### Verwendung der `yaml` PECL-Erweiterung
Falls gewünscht oder wenn es die Anforderungen Ihres Projekts zulassen, kann die PECL-Erweiterung eine weitere effiziente Möglichkeit sein, mit YAML zu arbeiten. Stellen Sie zunächst sicher, dass die Erweiterung installiert ist:

```bash
pecl install yaml
```

Aktivieren Sie sie dann in Ihrer `php.ini`-Konfiguration:

```ini
extension=yaml.so
```

So parsen und erzeugen Sie YAML:

```php
<?php

// YAML parsen
$yamlString = <<<YAML
greet: Hallo, Welt!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Erstellen von YAML aus einem Array
$array = [
    'greet' => 'Hallo, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

Die Ausgabe wird der der Symfony-Komponente ähnlich sein und veranschaulicht die Rolle von YAML als Brücke zwischen dem für Menschen lesbaren Format und PHP-Arraystrukturen, was eine einfachere Konfiguration und Datenhandhabung ermöglicht.
