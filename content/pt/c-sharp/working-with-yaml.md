---
title:                "Trabalhando com yaml"
html_title:           "C#: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com YAML é uma forma de organizar e armazenar dados de forma estruturada, em um formato de texto simples e fácil de ser lido por humanos. Programadores usam YAML para configurar e controlar ações em seus códigos, bem como para compartilhar informações entre diferentes linguagens e sistemas.

## Como fazer:

Para começar a utilizar YAML em seus projetos, é necessário adicionar a biblioteca "YamlDotNet" ao seu código. Em seguida, utilize a classe "YamlSerializer" para serializar e deserializar seus dados YAML. Veja um exemplo abaixo:

```C#
using YamlDotNet.Serialization; //adiciona a biblioteca

var serializer = new YamlSerializer(); //cria um novo serializador

var myData = new MyClass(); //cria uma nova classe com os dados que deseja armazenar
var yamlData = serializer.Serialize(myData); //serializa os dados para o formato YAML

Console.WriteLine(yamlData); //imprime o resultado no console
```

O código acima irá converter os dados em YAML e imprimi-los na tela. Para deserializar, basta utilizar o método "Deserialize" da classe "YamlSerializer". Essa é apenas uma forma básica de utilizar YAML, mas existem diversas outras opções e métodos disponíveis.

## Deep Dive

YAML foi criado em 2001 por Clark Evans e Ingy döt Net, e seu nome é um acrônimo para "YAML Ain't Markup Language". Ele foi desenvolvido como uma alternativa mais amigável ao XML, sendo mais legível e fácil de ser utilizado por humanos.

Existem algumas alternativas para trabalhar com dados estruturados, como JSON e XML. Cada um tem suas vantagens e desvantagens, e a escolha dependerá das necessidades do projeto. JSON é mais comumente utilizado em aplicações web, enquanto XML é mais utilizado para troca de dados entre sistemas.

Quando se trata de implementação de YAML, existem diferentes bibliotecas e ferramentas disponíveis para diferentes linguagens de programação. Além da biblioteca "YamlDotNet" em C#, há também outras opções populares como "PyYAML" em Python e "SnakeYAML" em Java.

## Veja também:

- [Documentação da biblioteca YamlDotNet](https://github.com/aaubry/YamlDotNet)