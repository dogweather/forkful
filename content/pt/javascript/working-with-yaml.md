---
title:                "Trabalhando com yaml"
html_title:           "Javascript: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML é uma linguagem simples e legível por humanos para representar dados estruturados. Com a sua sintaxe intuitiva, é uma escolha popular para arquivos de configuração e troca de dados entre diferentes sistemas. Ao aprender Javascript, entender como trabalhar com YAML pode ser útil para processar e manipular dados de forma eficiente.

## Como fazer

```Javascript
// Exemplo de objeto YAML
let frutas = `
- maçã
- banana
- pera
- laranja
- melão
`;

// Saída após conversão para array
let listaFrutas = YAML.parse(frutas);
console.log(listaFrutas); // ['maçã', 'banana', 'pera', 'laranja', 'melão']

// Exemplo de conversão de objeto Javascript para YAML
let carro = {
    marca: 'Tesla',
    modelo: 'Model 3',
    ano: 2021
}

console.log(YAML.stringify(carro)); // marca: Tesla, modelo: Model 3, ano: 2021

```

Para trabalhar com YAML em Javascript, é necessário primeiro instalar a biblioteca `js-yaml`. Em seguida, é possível usar métodos como `YAML.parse()` para converter uma string YAML em um objeto Javascript e `YAML.stringify()` para converter um objeto Javascript em uma string YAML. Isso torna mais fácil a manipulação e transferência de dados entre diferentes sistemas e aplicações.

## Mergulho aprofundado

O YAML, que significa "YAML Ain't Markup Language", foi criado em 2001 e é baseado em uma combinação de linguagem de marcação e linguagens de programação. Ele suporta tipos de dados como strings, números, objetos e arrays, e também permite a criação de chaves personalizadas para categorizar os dados. Além disso, é uma linguagem extensível e pode ser personalizada para atender às necessidades específicas de um projeto.

## Veja também

- [Documentação do YAML](https://yaml.org/)
- [Repositório da biblioteca `js-yaml`](https://github.com/nodeca/js-yaml)
- [Tutorial de YAML para iniciantes em Javascript](https://dev.to/mdenchev/understanding-yaml-4edd)