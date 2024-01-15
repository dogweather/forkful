---
title:                "Trabalhando com yaml"
html_title:           "PHP: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML, ou YAML Ain't Markup Language, é uma linguagem de serialização de dados que é fácil de ler e escrever para humanos e também pode ser interpretada diretamente por máquinas. É amplamente utilizado para armazenar configurações e dados estruturados em aplicativos web.

Trabalhar com YAML permite que os desenvolvedores tenham um formato claro e conciso para gravar e compartilhar dados, além de facilitar a integração de aplicativos com outras ferramentas e bibliotecas.

## Como fazer

A seguir, mostraremos alguns exemplos de como trabalhar com YAML em PHP, através de código e blocos de código no formato "PHP ...".

#### Ler um arquivo YAML

Para ler um arquivo YAML em PHP, podemos usar a função `yaml_parse_file()`.

```
<?php 

$file = 'config.yaml';

// lê o arquivo e converte para um array associativo
$config = yaml_parse_file($file);

// acessa os valores do array
echo $config['database']['username']; // output: admin
echo $config['database']['password']; // output: 123456
```

#### Escrever um arquivo YAML

Para escrever um arquivo YAML em PHP, podemos usar a função `yaml_emit()`.

```
<?php 

$data = [
    'title' => 'Meu novo post',
    'body' => 'Este é o conteúdo do meu novo post',
    'author' => 'João Silva'
];

// converte o array em uma string YAML
$yaml = yaml_emit($data);

// escreve a string no arquivo
file_put_contents('post.yaml', $yaml);
```

#### Converter JSON para YAML

Também é possível converter facilmente um objeto JSON para YAML utilizando as funções `json_decode()` e `yaml_emit()`.

```
<?php 

$json = '{"username":"admin","password":"123456"}';

// converte o JSON para um array associativo
$config = json_decode($json, true);

// converte o array para uma string YAML
$yaml = yaml_emit($config);

echo $yaml; // output: username: admin password: 123456
```

## Aprofundando

O YAML é baseado em um conjunto de padrões para definir estruturas de dados, como listas e dicionários. Para saber mais sobre a sintaxe e as regras do YAML, você pode consultar a especificação oficial em [yaml.org](http://yaml.org/spec/).

Além disso, existem diversas bibliotecas e ferramentas disponíveis para auxiliar no trabalho com YAML em PHP, como o [Symfony YAML Component](https://symfony.com/doc/current/components/yaml.html) e o [YAMLParser](https://github.com/chaseisabelle/yamlparser).

## Veja também

- [Documentação oficial do YAML](http://yaml.org/)
- [Symfony YAML Component](https://symfony.com/doc/current/components/yaml.html)
- [YAMLParser](https://github.com/chaseisabelle/yamlparser)