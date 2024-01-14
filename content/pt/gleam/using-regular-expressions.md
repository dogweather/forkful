---
title:    "Gleam: Utilizando expressões regulares"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Programação?

Expressões Regulares, também conhecidas como Regex, são sequências de caracteres que permitem pesquisar e validar padrões em strings de texto. Usá-las pode simplificar tarefas como encontrar e substituir palavras ou extrair informações específicas de um texto. Além disso, aprender a escrever e utilizar expressões regulares pode melhorar sua eficiência e tornar suas tarefas de programação mais flexíveis.

## Como Utilizar Expressões Regulares em Gleam

A linguagem de programação Gleam possui ferramentas poderosas para lidar com expressões regulares de forma eficiente. Para usar Regex em Gleam, primeiro devemos importar o módulo `Regex` para nosso código.

```Gleam
import Regex

// código de exemplo para criar uma expressão regular e usá-la para encontrar um padrão em uma string

let regex = Regex.compile("^Olá, (\\w+)") // cria uma expressão regular que procura pela palavra Olá seguida de um nome

case regex.capture("Olá, Maria") { // verifica se a string contém o padrão da expressão regular
  Some(match) -> //se o padrão for encontrado
    match.1 //retorna o nome capturado pelo grupo 1 (Maria)
  None -> "Não foi possível encontrar o nome" //se o padrão não for encontrado
}
```

Este é um exemplo simples de uso de expressões regulares em Gleam, mas há muitas outras funções e recursos disponíveis no módulo Regex que vale a pena explorar.

## Aprofundando-se em Expressões Regulares

Embora possam parecer intimidantes no início, expressões regulares são uma ferramenta poderosa para lidar com padrões em strings de texto. Se você deseja dominar efetivamente o uso de expressões regulares em sua programação, aqui estão algumas dicas para aprofundar seu conhecimento:

- Comece com o básico: Familiarize-se com os elementos básicos das expressões regulares, como metacaracteres, classes de caracteres e quantificadores. Essas construções são a base para criar padrões.

- Use expressões regulares online: Existem muitas ferramentas online que permitem testar e experimentar diferentes expressões regulares em tempo real, o que pode ajudá-lo a entender melhor como elas funcionam.

- Pratique: A melhor maneira de se tornar confortável com expressões regulares é praticar, então tente resolver problemas de programação que exigem seu uso e explore os diferentes recursos da linguagem.

## Veja Também

- [Documentação do Módulo Regex em Gleam](https://gleam.run/modules/regex.html)
- [Guia Interativo para Expressões Regulares (em Inglês)](https://regexone.com/)
- [Testador de Expressões Regulares Online (em Português)](https://www.regexpal.com/)

Agora que você tem uma introdução básica às expressões regulares em Gleam, aproveite para explorar e experimentar com esta ferramenta poderosa em suas tarefas de programação. Com prática e dedicação, você pode se tornar um mestre em Regex em pouco tempo!