---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:21.080577-07:00
description: "Como Fazer: PHP, em suas itera\xE7\xF5es atuais, n\xE3o suporta a an\xE1\
  lise de YAML como parte de sua biblioteca padr\xE3o. A maneira mais direta de trabalhar\
  \ com YAML\u2026"
lastmod: '2024-03-13T22:44:46.687457-06:00'
model: gpt-4-0125-preview
summary: "PHP, em suas itera\xE7\xF5es atuais, n\xE3o suporta a an\xE1lise de YAML\
  \ como parte de sua biblioteca padr\xE3o."
title: Trabalhando com YAML
weight: 41
---

## Como Fazer:
PHP, em suas iterações atuais, não suporta a análise de YAML como parte de sua biblioteca padrão. A maneira mais direta de trabalhar com YAML em PHP é usando o componente YAML do Symfony ou a extensão PECL `yaml`.

### Usando o Componente YAML do Symfony
Primeiro, instale o componente YAML do Symfony via Composer:

```bash
composer require symfony/yaml
```

Então, você pode analisar e despejar conteúdo YAML da seguinte maneira:

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// Analisando YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// Criando YAML a partir de um array
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

Saída de amostra ao analisar:

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

Saída de amostra ao despejar:

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### Usando a Extensão `yaml` PECL
Se preferir, ou se os requisitos do seu projeto permitirem, a extensão PECL pode ser outra maneira eficiente de trabalhar com YAML. Primeiro, certifique-se de que a extensão esteja instalada:

```bash
pecl install yaml
```

Então, habilite-a na sua configuração `php.ini`:

```ini
extension=yaml.so
```

Veja como analisar e emitir YAML:

```php
<?php

// Analisando YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// Criando YAML a partir de um array
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

A saída será similar à do componente do Symfony, ilustrando o papel do YAML como uma ponte entre o formato legível por humanos e as estruturas de array do PHP, facilitando a configuração e o manuseio de dados mais fácil.
