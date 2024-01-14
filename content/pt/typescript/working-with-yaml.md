---
title:                "TypeScript: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

YAML (Yet Another Markup Language) é uma linguagem de marcação leve e fácil de ler, usada principalmente para criar configurações e estruturas de dados. Com o uso cada vez mais comum de sistemas de automação e orquestração de TI, como o Ansible e o Kubernetes, trabalhar com YAML se tornou uma habilidade valiosa para desenvolvedores e administradores de sistemas.

## Como fazer

Para trabalhar com YAML em projetos TypeScript, é necessário ter o pacote "js-yaml" instalado. Com ele, podemos ler e escrever arquivos YAML, bem como converter objetos JavaScript em YAML e vice-versa.

```
import * as YAML from 'js-yaml';

// Lendo um arquivo YAML
const dadosYAML = YAML.safeLoad(fs.readFileSync('config.yaml', 'utf8'));

// Convertendo um objeto JavaScript em YAML
const dadosObjeto = { nome: 'João', idade: 25 };
const dadosConvertidos = YAML.safeDump(dadosObjeto);

console.log(dadosConvertidos);
// Saída: "nome: João\nidade: 25\n"
```

## Mergulho profundo

Além das funções básicas de leitura e escrita, also é possível utilizar o pacote "js-yaml" para tarefas mais avançadas, como a validação de arquivos YAML e a utilização de "tags" personalizadas.

O YAML possui uma série de regras de sintaxe que devem ser seguidas para que o arquivo seja válido. O pacote "js-yaml" conta com a função "YAML.validate" que permite verificar se um arquivo atende a essas regras e está bem formatado.

Além disso, é possível utilizar tags personalizadas para diferenciar objetos e tipos de dados dentro do arquivo YAML. Para isso, basta usar a função "YAML.tagged" e fornecer as definições das tags que deseja utilizar.

## Veja também

- [Documentação oficial do pacote "js-yaml"](https://www.npmjs.com/package/js-yaml)
- [Tutorial de YAML para iniciantes](https://learnxinyminutes.com/docs/yaml/)
- [Exemplos de uso do YAML em projetos TypeScript](https://github.com/topics/yaml-typescript)