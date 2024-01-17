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

# Introdução ao Uso de YAML em PHP

## O que é e por que trabalhar com YAML

YAML (YAML Ain't Markup Language) é uma linguagem de serialização de dados que permite a criação de arquivos de configuração de forma mais simples e legível do que outros formatos, como o JSON ou o XML. Programadores utilizam o YAML porque ele oferece uma forma mais eficiente e organizada de armazenar dados estruturados.

## Como usar YAML em PHP

Para usar YAML em PHP, é necessário primeiro instalar a extensão `yaml` do PHP. Em seguida, você pode utilizar a função `yaml_parse()` para ler um arquivo YAML e convertê-lo em um array associativo, que pode ser facilmente manipulado pelo seu código.

```
<?php
// Abrir arquivo YAML
$arquivo = file_get_contents('config.yml');

// Converter para array
$config = yaml_parse($arquivo);

// Acessar valores do array
echo $config['banco_de_dados']['host'];

// Saída: localhost
?>
```

## Mergulho Profundo em YAML

YAML foi criado em 2001 por Clark Evans e Ingy döt Net com o objetivo de ser uma linguagem mais legível e fácil de usar para criação de arquivos de configuração. Além disso, ele é baseado em dois princípios: ser fácil para humanos lerem e escreverem, e ser fácil de ser processado por máquinas.

Existem alternativas para trabalhar com dados estruturados em PHP, como o JSON e o XML. No entanto, o YAML se destaca pela sua simplicidade e facilidade de leitura. Além disso, ele possui recursos avançados, como referências e heranças, que facilitam a criação de configurações complexas.

Na implementação de YAML em PHP, a extensão `yaml` utiliza a biblioteca libyaml, escrita em C, para fazer a conversão do formato YAML para o formato PHP. Isso garante uma alta performance e, ao mesmo tempo, mantém a sintaxe legível e concisa do YAML.

## Veja também

- Página oficial do YAML (https://yaml.org/)
- Documentação oficial da extensão yaml no PHP (https://www.php.net/manual/en/book.yaml.php)
- Tutorial de como usar YAML em PHP (https://www.phpzag.com/read-and-write-yaml-files-in-php/)