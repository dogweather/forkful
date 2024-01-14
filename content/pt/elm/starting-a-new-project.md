---
title:    "Elm: Iniciando um novo projeto"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Por que começar um novo projeto em Elm?

Se você está procurando por uma linguagem de programação funcional e altamente escalável para o seu próximo projeto, então Elm pode ser a escolha certa. Com uma sintaxe simples e uma forte ênfase em garantir a segurança do seu código, Elm é cada vez mais popular entre os desenvolvedores.

## Como fazer

Antes de começar a codificar em Elm, certifique-se de ter instalado o compilador através do Node.js. Em seguida, crie um novo projeto com o comando ```elm init```. Agora você está pronto para começar!

Um exemplo simples de código Elm seria criar uma lista de números pares até 10. Veja como isso pode ser realizado em Elm:

```
module Main exposing (main)

import Html exposing (text)

-- Definir a função para gerar a lista
listadePares : List Int
listadePares =
    List.range 2 10

main =
    -- Imprimir o resultado em uma tag HTML
    text (toString listadePares)
```

A saída seria: [ 2, 4, 6, 8, 10 ]. Com apenas algumas linhas de código, você já está progredindo em sua jornada com Elm!

## Aprofundando-se

Uma das principais vantagens de usar Elm é sua forte tipagem estática. Isso significa que, durante a compilação, os erros e bugs em potencial são revelados antes mesmo de executar o código. Isso aumenta a segurança e eficiência do seu programa.

Outra característica interessante do Elm é sua arquitetura de aplicativos (The Elm Architecture). Ela é baseada em três componentes principais: model (modelo), update (atualização) e view (visualização). Esses componentes trabalham juntos para criar uma estrutura clara e organizada que facilita a manutenção e expansão do seu código.

# Veja também

Para mais informações sobre Elm e como começar a usá-lo, confira esses recursos úteis:

- [Documentação oficial](https://guide.elm-lang.org/)
- [Elm Brasil](https://elm-brasil.org/)
- [Fórum Elm na Discuss](https://discourse.elm-lang.org/)
- [Github Elm](https://github.com/elm/)