---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
YAML, c'est pour dire "YAML Ain't Markup Language". Les devs utilisent YAML car c'est simple à comprendre et à modifier, parfait pour configurer des projets ou définir des données.

## How to:
Pour travailler avec YAML en PHP :

1. Installer le parseur YAML via Composer :
```bash
composer require symfony/yaml
```
2. Lire un fichier YAML :
```php
<?php
use Symfony\Component\Yaml\Yaml;

$yaml = Yaml::parse(file_get_contents('config.yml'));
print_r($yaml);
?>
```
3. Écrire en YAML :
```php
<?php
use Symfony\Component\Yaml\Yaml;

$array = ['foo' => 'bar', 'bar' => 'baz'];
$yaml = Yaml::dump($array);

file_put_contents('config.yml', $yaml);
?>
```
## Deep Dive
YAML est sorti en 2001. C'est une alternative à XML et JSON. En PHP, la librairie la plus connue pour gérer YAML est Symfony Yaml, mais il existe yaml pecl extension pour une approche plus directe si on préfère.

## See Also
- Documentation de Symfony Yaml : https://symfony.com/doc/current/components/yaml.html
- YAML officiel : https://yaml.org/
- Extension PECL YAML : https://pecl.php.net/package/yaml