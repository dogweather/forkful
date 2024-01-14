---
title:                "Gleam: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares no Gleam?

As expressões regulares são uma ferramenta poderosa para processar e manipular texto. E no contexto do Gleam, elas podem ser usadas para validar entradas, extrair dados específicos de uma string, ou até mesmo facilitar a construção de algoritmos mais complexos. Com as expressões regulares, é possível economizar tempo e esforço ao lidar com tarefas relacionadas à manipulação de texto.

## Como usar Expressões Regulares em Gleam

Para usar expressões regulares em Gleam, é necessário o conhecimento básico da sintaxe de expressões regulares. Depois disso, basta utilizar o módulo `regex` do Gleam, que fornece funções para criar e executar as expressões regulares. Veja um exemplo de como validar um endereço de e-mail usando expressões regulares em Gleam:

````Gleam
import regex

let email_regex = regex.compile("^\\w+@[a-z]+\\.[a-z]{2,}$") // cria a expressão regular

fn validar_email(email) {
  result = regex.matches(email_regex, email) // executa a expressão regular no email
  
  case result {
    Ok(_) -> "Endereço de e-mail válido."
    Error(_) -> "Endereço de e-mail inválido."
  }
}

email = "joao@email.com"

validar_email(email) // output: "Endereço de e-mail válido."
````

## Navegando pelas Expressões Regulares

Existem diversos recursos e padrões que podem ser utilizados nas expressões regulares, como classes de caracteres, quantificadores, capturas de grupos e muito mais. Por isso, é importante ter um bom entendimento de como esses elementos funcionam para criar expressões regulares eficazes. Além disso, é recomendado testar e verificar as expressões regulares em sites como o [Regex101](https://regex101.com/) antes de usá-las no código.

## Veja também

- [Documentação oficial do módulo Regex no Gleam](https://gleam.run/modules/regex.md)
- [Tutorial sobre expressões regulares em Gleam](https://gleam.run/book/tutorials/regular-expressions.html)
- [Página de cheatsheet de expressões regulares em Gleam](https://gleam.run/cms/docs/regular-expressions/)
- [Livro "Aprenda a Programar com Gleam", que aborda expressões regulares em um de seus capítulos](https://www.aprendaprog.com.br/livro/gleam/)