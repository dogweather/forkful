---
title:                "Iniciando um novo projeto"
html_title:           "Elm: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Elm?

Se você está à procura de uma linguagem de programação funcional, que seja fácil de aprender e que ofereça alta performance, o Elm pode ser a solução ideal para o seu próximo projeto. Além disso, a sua sintaxe declarativa e a robustez do sistema de tipos ajudam a garantir que o código seja mais previsível e menos propenso a erros.

## Como começar um projeto em Elm

Para começar, é necessário ter o Elm instalado em seu computador. A forma mais simples de fazer isso é por meio do npm, o gerenciador de pacotes do Node.js. Basta digitar o seguinte comando em seu terminal:

```Elm
npm install -g elm
```

Com o Elm instalado, podemos criar um novo projeto utilizando o comando `elm init`, que irá gerar uma estrutura de arquivos básica para o nosso projeto.

Dentro da pasta do projeto, podemos criar um arquivo `Main.elm` que será o ponto de partida para a nossa aplicação. Nele, podemos importar os módulos necessários e definir as funções e tipos que serão utilizados.

```Elm
module Main exposing (..)

import Html exposing (text)

main = text "Hello, world!"
```

Para compilar o nosso código, basta executar o comando `elm make src/Main.elm`, que irá gerar um arquivo `index.html` com o nosso código pronto para ser executado.

## Uma visão mais profunda sobre a criação de um projeto em Elm

Ao começar um projeto em Elm, é importante entender os conceitos fundamentais da linguagem, como funções puras, tipagem estática e imutabilidade. Também é importante conhecer as ferramentas disponíveis, como o debugger integrado, que ajuda a encontrar e corrigir erros de forma mais eficiente.

Além disso, é recomendado seguir as práticas recomendadas pela comunidade, como utilizar o buscador de pacotes `elm-package`, que facilita a instalação de bibliotecas, e realizar testes unitários para garantir a qualidade do código.

## Veja também

- [Documentação oficial do Elm](https://guide.elm-lang.org/)
- [Elm Casts - vídeos sobre Elm em português](https://www.youtube.com/playlist?list=PLbvnARcdVmgDgGbM9MsWNzMwoR1BCoB7o)
- [Exemplos de projetos em Elm](https://github.com/elm/projects)