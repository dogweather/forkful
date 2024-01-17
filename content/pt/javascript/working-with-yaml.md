---
title:                "Trabalhando com o Yaml"
html_title:           "Javascript: Trabalhando com o Yaml"
simple_title:         "Trabalhando com o Yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que? Por que?
Trabalhar com YAML é uma forma de representar dados de forma legível tanto para humanos quanto para máquinas. Programadores utilizam YAML para criar arquivos de configuração e estruturar dados em seus projetos, pois é uma linguagem simples e flexível.

## Como fazer:
Para utilizar YAML em seu código Javascript, é necessário primeiro instalar o pacote `js-yaml` através do gerenciador de pacotes NPM. Depois, basta importar o pacote utilizando `require` e utilizar suas funções para ler e escrever arquivos YAML. Veja um exemplo básico abaixo:

```Javascript
const yaml = require('js-yaml');
// Lendo um arquivo YAML
const dadosYAML = yaml.load('config.yml');
// Escrevendo um arquivo YAML
const dados = {
  nome: 'Ana',
  idade: 30,
};
yaml.dump(dados, 'perfil.yml');
```

O pacote `js-yaml` possui diversas outras funções e opções para trabalhar com YAML em seu código Javascript. Consulte a documentação oficial para mais detalhes.

## Profundidade
YAML (acrônimo para "YAML Ain't Markup Language") foi criado em 2001 por um grupo de desenvolvedores e tem como objetivo ser uma linguagem mais amigável e intuitiva que o XML. Além disso, existem outras alternativas para trabalhar com estruturação de dados, como o formato JSON, por exemplo. A implementação do pacote `js-yaml` utiliza a biblioteca `libyaml`, que é escrita em C e otimizada para performance.

## Veja também
- [Documentação oficial do pacote js-yaml](https://github.com/nodeca/js-yaml)
- [Website oficial do YAML](https://yaml.org)
- [Outras alternativas para estruturação de dados](https://www.freecodecamp.org/news/json-vs-xml-which-is-the-best-data-format/)