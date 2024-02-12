---
title:                "Organizando o código em funções"
aliases:
- /pt/elm/organizing-code-into-functions/
date:                  2024-01-26T01:10:34.654351-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Jogar todo o seu código em uma grande pilha? Má ideia. Dividi-lo em funções? Boa ideia. Isso mantém o seu código em Elm limpo, reutilizável e mais fácil de testar. Ao organizar seu código em funções, você agrupa o código que executa tarefas específicas juntas, o que torna sua aplicação mais fácil de manter e entender.

## Como fazer:
Aqui está um trecho de código Elm com uma função simples para cumprimentar um usuário:

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Olá, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

Execute-o, e você terá a saída: "Olá, Casey!"

Agora, digamos que você queira adicionar mais personalização. Extraia mais funcionalidades!

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Howdy" userName

main =
    text (personalGreeting "Casey")
```

Agora, quando executar: "Howdy, Casey!" Magia? Não, apenas funções fazendo o seu trabalho.

## Mergulho Profundo
Antigamente, o código era frequentemente uma longa sequência de instruções (pense em código espaguete). Era um pesadelo para manter. Então veio a programação estruturada e, com ela, as funções. Elm, como seus predecessores de programação funcional, depende fortemente de funções para organização.

Você pode aninhar funções, criando fechamentos, ou mantê-las puras para simplicidade. Elm encoraja o último: funções puras com entradas e saídas bem definidas, levando a uma depuração e teste mais fáceis.

As funções de Elm também podem ser de ordem superior, o que significa que elas podem aceitar ou retornar outras funções. Isso abre um mundo de composabilidade. No entanto, ao contrário de algumas outras línguas, Elm não possui sobrecarga de função; cada função deve ter um nome único.

Além disso, Elm impõe um sistema de tipagem estática forte que não só verifica os tipos, mas também os infere, reduzindo o código redundante.

Comparado com alternativas como organização de código procedural ou orientado a objetos em outras línguas, a abordagem do Elm enfatiza simplicidade e previsibilidade. Elm não tem objetos ou classes. Você organiza o código com funções e módulos ao invés de classes e instâncias.

## Veja Também
Para explorar mais a fundo, confira estes recursos:
- Guia oficial de Elm sobre funções: https://guide.elm-lang.org/core_language.html
- Documentação de pacote Elm para exemplos de funções mais complexas: https://package.elm-lang.org/
- Saiba sobre o sistema de tipos de Elm, que se alinha bem com a organização de funções: https://elm-lang.org/docs/types
