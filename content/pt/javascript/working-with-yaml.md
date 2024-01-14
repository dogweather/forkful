---
title:                "Javascript: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML (acrônimo de *YAML Ain't Markup Language*) é uma linguagem de serialização de dados que permite armazenar informações de forma estruturada e legível tanto para humanos quanto para máquinas. É amplamente utilizado na programação para configurar e gerenciar aplicações e projetos. Trabalhar com YAML pode tornar o processo de desenvolvimento mais eficiente e fácil de manter.

## Como utilizar o YAML em seu código Javascript

Para começar a trabalhar com YAML em seu código Javascript, primeiro é necessário instalar a biblioteca js-yaml através do gerenciador de pacotes NPM. Depois, basta importar a biblioteca em seu código e utilizar suas funções para fazer a leitura e escrita de arquivos YAML. Veja um exemplo de leitura de um arquivo YAML e impressão de seu conteúdo:

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

// lê o arquivo 'config.yml'
const config = yaml.safeLoad(fs.readFileSync('config.yml', 'utf8'));

// imprime o valor da chave 'database'
console.log(config.database);
```
### Saída:

```
mongodb://localhost:27017/my_database
```

## Mergulho Profundo no YAML

YAML é baseado em uma estrutura de dados de chave-valor e suporta vários tipos de dados, como strings, números, listas e objetos. Além disso, ele permite o uso de comentários e referências para evitar a repetição de informação. É uma linguagem bastante flexível e pode ser utilizada em conjunto com outras linguagens de marcação, como JSON e XML.

Algumas dicas para trabalhar com YAML:
- Utilize indentação consistente: a indentação é fundamental para definir a hierarquia dos dados em YAML.
- Use aspas duplas para strings com espaços: strings que contêm espaços devem ser escritas entre aspas duplas.
- Tenha atenção aos espaços em branco: espaços em branco desnecessários podem causar erros ao ler o arquivo YAML.

## Veja Também

- [Documentação oficial do YAML](https://yaml.org/)
- [Página da biblioteca js-yaml no NPM](https://www.npmjs.com/package/js-yaml)
- [Tutorial de como utilizar YAML em Javascript](https://medium.com/devsquads/working-with-yaml-files-in-js-be899a268f44)