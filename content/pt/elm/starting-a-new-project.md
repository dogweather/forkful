---
title:                "Iniciando um novo projeto"
aliases:
- pt/elm/starting-a-new-project.md
date:                  2024-01-20T18:03:21.775298-07:00
model:                 gpt-4-1106-preview
simple_title:         "Iniciando um novo projeto"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Começar um novo projeto em Elm é inicializar um ambiente de desenvolvimento do zero, configurando tudo o que você precisa para escrever seu código Elm. Programadores fazem isso para criar aplicações web confiáveis e manuteníveis com uma linguagem que privilegia a simplicidade e a qualidade do código.

## Como Fazer:
Para iniciar um novo projeto Elm, você precisa ter o Elm instalado. Se ainda não tem, visite [elm-lang.org](https://elm-lang.org/) para instruções de instalação. Agora, vamos criar um projeto simples:

```Elm
-- Instale o Elm na sua máquina e depois confirme a versão
$ elm --version

-- Crie uma nova pasta para o seu projeto e entre nela
$ mkdir meu-projeto-elm
$ cd meu-projeto-elm

-- Inicie um novo projeto Elm
$ elm init
```

Isso criará um `elm.json` que gerencia as dependências do seu projeto. Para escrever o seu primeiro programa, crie um arquivo chamado `Main.elm` com o seguinte conteúdo:

```Elm
module Main exposing (main)

import Html exposing (text)

main =
    text "Olá, mundo Elm!"
```

Para compilar seu arquivo Elm em um arquivo JavaScript, use o seguinte comando:

```Elm
$ elm make Main.elm --output main.js
```

Isso gera um arquivo `main.js` que você pode incluir em uma página HTML para ver o seu programa em ação.

## Mergulho Profundo
A linguagem Elm surgiu com o objetivo de tornar a programação front-end mais fácil e mais robusta, oferecendo um sistema de tipos forte e uma arquitetura que evita erros em tempo de execução. Enquanto JavaScript é a linguagem de escolha para muitos, Elm oferece uma alternativa que se destaca por não ter exceções em tempo de execução e por suas mensagens de erro amigáveis, ajudando na manutenção e escalabilidade das aplicações.

Comparado com alternativas como React ou Angular, Elm promove um padrão arquitetônico único, conhecido como The Elm Architecture, que enfatiza a separação clara entre o modelo, as atualizações e as visualizações.

Implementar um projeto Elm geralmente envolve menos configuração que outros frameworks JavaScript modernos. Por ser uma linguagem compilada, muitos dos problemas potenciais são eliminados durante a compilação, antes mesmo do código ir para produção.

## Veja Também
- Documentação Oficial Elm: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- Elm Architecture Tutorial: [https://guide.elm-lang.org/architecture/](https://guide.elm-lang.org/architecture/)
- Exemplos de código Elm: [https://elm-lang.org/examples](https://elm-lang.org/examples)
