---
title:                "Refatoração"
date:                  2024-01-26T01:18:18.802559-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Refatoração é essencialmente uma limpeza de primavera no seu código — trata-se de reestruturar o código existente sem alterar seu comportamento externo. Os programadores fazem isso para tornar o código mais legível, reduzir a complexidade, melhorar a manutenção e facilitar a extensão.

## Como Fazer:
Considere que você tem uma função Elm que está fazendo demais, como misturar lógica de UI com atualizações de estado. É um candidato perfeito para refatoração. Originalmente:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

Depois da refatoração, separamos as preocupações puxando a lógica para diferentes funções:

```Elm
-- A lógica de atualização está separada
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- A lógica de formatação (view) também está separada
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Limpa a entrada se for muito curta, como uma regra de exemplo.

-- A função de atualização agora usa funções auxiliares
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Com essas mudanças, você tem uma clara separação, e cada função fica mais fácil de entender e testar.

## Aprofundamento
Refatoração como uma prática formal pode ser rastreada até os primórdios da programação, quando o custo de mudar o código já estava sendo reconhecido como um aspecto crítico do processo de desenvolvimento. Notavelmente, o livro de Martin Fowler "Refatoração: Aperfeiçoando o Design de Códigos Existentes", publicado no final dos anos 90, realmente preparou o terreno para a refatoração com uma abordagem estruturada e um catálogo de "cheiros de código" para identificar oportunidades de refatoração.

No contexto de Elm, a refatoração aproveita as forças da linguagem, como seu forte sistema de tipos, que promove confiança durante o processo. Alternativas para refatoração manual podem incluir ferramentas automatizadas de transformação de código, mas as ferramentas de Elm nessa área ainda estão amadurecendo comparadas a algumas linguagens mais antigas. Detalhes de implementação muitas vezes giram em torno de refatorações comuns como extração de função, renomeação e simplificação de condicionais. O compilador de Elm é um aliado chave na refatoração, pois ele não permite que você se desvie muito — ele alerta sempre que algo está errado, garantindo que seu código refatorado ainda funcione.

## Veja Também
- ["Refatoração: Aperfeiçoando o Design de Códigos Existentes" por Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Fórum de Elm - Tópicos sobre Refatoração](https://discourse.elm-lang.org/search?q=refactoring)
