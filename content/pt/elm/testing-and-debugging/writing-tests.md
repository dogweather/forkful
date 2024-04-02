---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:40.983301-07:00
description: "Escrever testes em Elm envolve a cria\xE7\xE3o de casos de teste para\
  \ verificar a corre\xE7\xE3o do seu c\xF3digo Elm, garantindo que ele se comporte\
  \ conforme esperado.\u2026"
lastmod: '2024-03-13T22:44:46.503744-06:00'
model: gpt-4-0125-preview
summary: "Escrever testes em Elm envolve a cria\xE7\xE3o de casos de teste para verificar\
  \ a corre\xE7\xE3o do seu c\xF3digo Elm, garantindo que ele se comporte conforme\
  \ esperado.\u2026"
title: Escrevendo testes
weight: 36
---

## O que & Por quê?

Escrever testes em Elm envolve a criação de casos de teste para verificar a correção do seu código Elm, garantindo que ele se comporte conforme esperado. Os programadores fazem isso para capturar bugs precocemente, facilitar a manutenção e melhorar a qualidade e confiabilidade de suas aplicações.

## Como fazer:

Elm utiliza o pacote `elm-explorations/test` para escrever testes unitários e de fuzz. Comece adicionando o pacote ao seu projeto:

```elm
elm install elm-explorations/test
```

Crie um arquivo de teste, digamos `tests/ExampleTest.elm`, e importe os módulos de teste. Aqui está um teste simples que verifica uma função `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Uma simples função de adição"
        [ test "Adicionar 2 e 3 resulta em 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Para executar seus testes, você precisará de `elm-test`:

```shell
npm install -g elm-test
elm-test
```

Isso irá compilar seus testes e imprimir os resultados no seu terminal. Para o exemplo acima, a saída deve ser algo como:

```
CORRIDA DE TESTES APROVADA

Duração: 42 ms
Passaram:   1
Falharam:   0
```

Para um exemplo mais complexo, digamos que você queira fazer um teste de fuzz na função `add` para garantir que ela lide corretamente com uma ampla gama de entradas de inteiros. Você modificaria o seu `ExampleTest.elm` da seguinte forma:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testando add com fuzzing"
        [ fuzz int "Teste de fuzz em add com inteiros aleatórios" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Execute `elm-test` novamente para ver os testes de fuzz em ação. A saída variará com a entrada aleatória, mas testes bem-sucedidos indicarão a ausência de falhas:

```
CORRIDA DE TESTES APROVADA

Duração: 183 ms
Passaram:   100
Falharam:   0
``` 

Estes exemplos mostram como escrever e executar testes unitários e de fuzz simples em Elm, usando o pacote `elm-explorations/test`. Testar é uma parte vital do processo de desenvolvimento, ajudando a garantir que suas aplicações Elm sejam confiáveis e mantenham alta qualidade.
