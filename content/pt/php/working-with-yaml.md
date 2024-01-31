---
title:                "Trabalhando com YAML"
date:                  2024-01-19
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

YAML, que significa "YAML Ain't Markup Language", é um formato de serialização de dados humano-legível usado para configurações, armazenamento de dados e comunicação entre serviços. Programadores utilizam YAML pela sua facilidade de leitura e escrita quando comparado com outros formatos, como XML ou JSON.

## Como Fazer:

Vamos começar lendo e escrevendo YAML em PHP. Primeiro, instale a biblioteca yaml com `composer require symfony/yaml`.

### Ler YAML:
```php
<?php
require 'vendor/autoload.php';
use Symfony\Component\Yaml\Yaml;

$yamlContent = <<<YAML
nome: João
idade: 30
linguagens:
  - PHP
  - JavaScript
  - Python
YAML;

$array = Yaml::parse($yamlContent);
print_r($array);
```

**Saída:**
```
Array
(
    [nome] => João
    [idade] => 30
    [linguagens] => Array
        (
            [0] => PHP
            [1] => JavaScript
            [2] => Python
        )

)
```

### Escrever YAML:
```php
<?php
require 'vendor/autoload.php';
use Symfony\Component\Yaml\Yaml;

$array = [
  'nome' => 'Maria',
  'idade' => 25,
  'linguagens' => ['PHP', 'Ruby']
];

$yaml = Yaml::dump($array);
echo $yaml;
```

**Saída:**
```
nome: Maria
idade: 25
linguagens:
  - PHP
  - Ruby
```

## Aprofundamento

YAML foi introduzido em 2001 e rapidamente adotado por causa da sua acessibilidade. Apesar de JSON ser comumente utilizado para APIs e configurações por ser mais conciso, YAML ainda é a escolha preferencial para arquivos de configuração devido à sua legibilidade. Cuidado com a tabulação: YAML exige espaços, não tabs.

Alternativas populares ao YAML incluem JSON e XML, porém muitas ferramentas de DevOps, como Docker e Kubernetes, utilizam YAML para arquivos de configuração. A implementação em PHP se faz através da extensão `ext-yaml` ou da biblioteca `symfony/yaml`, que oferece mais flexibilidade e compatibilidade.

## Veja Também

- Documentação oficial Symfony Yaml Component: https://symfony.com/doc/current/components/yaml.html
- YAML: https://yaml.org/
- Tutorial YAML: https://www.tutorialspoint.com/yaml/index.htm
- A comparação entre YAML, JSON e XML: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
