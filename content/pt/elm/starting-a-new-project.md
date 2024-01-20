---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Começando um novo projeto em Elm

## What & Why?

Iniciar um novo projeto é a criação de um novo aplicativo ou sistema do zero. Programadores fazem isso para desenvolver soluções customizadas, criar novos produtos, ou aprender novas habilidades.

## Como fazer:

Usamos a ferramenta `elm init` para começar um novo projeto Elm. Vamos ver um exemplo:

```Elm
$ mkdir meu_projeto
$ cd meu_projeto
$ elm init
```

Este comando criará uma nova pasta na sua aplicação chamada `src` e um arquivo `elm.json`.

`elm.json` é um resumo do seu projeto e tem uma aparência similar a essa:

```Elm
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {},
        "indirect": {}
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
```

Para escrever seu primeiro código, você pode criar um arquivo chamado `Main.elm` no diretório `src`.

## Mergulho profundo

Elm tem um histórico interessante. Foi criado por Evan Czaplicki em 2012 como sua tese de mestrado. A ideia era criar uma linguagem funcional que compilasse para JavaScript, proporcionando um ambiente previsível e debugável.

Em termos de alternativas, existem outras linguagens que compilam para JavaScript, como PureScript e ReasonML. No entanto, Elm é conhecido por sua simplicidade e facilidade de uso.

Quando você inicializa um novo projeto Elm com `elm init`, a ferramenta realiza várias ações nos bastidores. Cria uma nova pasta para o projeto, gera um arquivo `elm.json` com as configurações do projeto e prepara um diretório para seus arquivos de código.

## Veja também

1. [Documentação oficial do Elm](https://elm-lang.org/docs)
2. [Como começar com o Elm](https://guide.elm-lang.org/install/)
3. [Repositório GitHub do Elm](https://github.com/elm/)
4. [Elm no Exercism](https://exercism.io/tracks/elm) - exercícios práticos para aprender Elm