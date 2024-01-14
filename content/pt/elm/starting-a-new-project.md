---
title:    "Elm: Começando um novo projeto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto em Elm?

Se você está buscando uma linguagem funcional moderna e robusta para o desenvolvimento de front-end, então o Elm é uma ótima escolha. Ele é conhecido por sua sintaxe simples e fácil de aprender, além de ter um sistema de tipos forte para garantir código mais seguro e menos erros. Além disso, a comunidade de desenvolvedores de Elm está crescendo e oferece uma ampla gama de recursos e ferramentas para suporte e colaboração.

## Como começar um projeto em Elm

Primeiramente, é necessário ter o Elm instalado em seu sistema. Em seguida, utilize o comando `elm init` para criar uma estrutura básica de projeto. Se estiver utilizando algum ambiente de desenvolvimento como o VS Code, é recomendável instalar a extensão Elm para uma melhor experiência de codificação. Agora, vamos criar um pequeno programa que irá imprimir "Hello World!" na tela:

```Elm
module Main exposing (main)

import Html exposing (text)

main =
  text "Hello World!"
```

Para executar o programa, basta digitar `elm reactor` no terminal e acessar `localhost:8000` em seu navegador. Você verá a mensagem "Hello World!" na tela.

## Mergulho profundo

Ao começar um novo projeto em Elm, é importante seguir as práticas recomendadas pela comunidade e utilizar ferramentas populares, como o Elm UI e o Elm CSS. Além disso, tenha como meta manter seu código limpo e bem organizado para evitar problemas futuros. Fazer testes regulares também é essencial para garantir a qualidade do código.

## Veja também

- Documentação Oficial do Elm: https://guide.elm-lang.org/
- Repositório do Elm UI: https://github.com/rtfeldman/elm-css
- Comunidade Elm no Reddit: https://www.reddit.com/r/elm/