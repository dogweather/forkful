---
date: 2024-01-26 01:03:15.155397-07:00
description: "Registrar (logging) \xE9 essencialmente o processo de grava\xE7\xE3\
  o de eventos e sa\xEDdas de dados de um software enquanto ele est\xE1 em execu\xE7\
  \xE3o, pense nisso como o\u2026"
lastmod: 2024-02-19 22:05:05.545366
model: gpt-4-1106-preview
summary: "Registrar (logging) \xE9 essencialmente o processo de grava\xE7\xE3o de\
  \ eventos e sa\xEDdas de dados de um software enquanto ele est\xE1 em execu\xE7\xE3\
  o, pense nisso como o\u2026"
title: Registro de Logs
---

{{< edit_this_page >}}

## O Que & Por Quê?
Registrar (logging) é essencialmente o processo de gravação de eventos e saídas de dados de um software enquanto ele está em execução, pense nisso como o diário do software. Os programadores usam o registro para acompanhar o que está acontecendo por debaixo dos panos - é inestimável para depurar problemas, monitorar o comportamento do sistema em tempo real e analisar atividades passadas para otimizações de desempenho ou auditorias.

## Como fazer:
A arquitetura do Elm não suporta efeitos colaterais como o registro imediatamente - você os gerencia por meio de comandos, que são uma parte da arquitetura da sua aplicação. Para fins educativos, vamos ver como você poderia simular o registro enviando mensagens para o JavaScript através de portas.

Primeiro, você vai definir um módulo de porta:

```Elm
port module Logger exposing (..)

-- Definir uma porta para enviar registros para o JavaScript
port log : String -> Cmd msg
```

No seu `Main.elm`, você usaria a porta `log` para enviar uma mensagem de registro:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- algumas atualizações no seu modelo aqui
            ( updatedModel, log "Ocorreu um AnEvent." )

        AnotherEvent ->
            -- outras atualizações de modelo aqui
            ( anotherUpdatedModel, log "Ocorreu um AnotherEvent." )
```

No lado do JavaScript, você assinaria a porta `log` para gerenciar as mensagens de registro que chegam:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

A saída de amostra no console do JavaScript seria então:

```
Ocorreu um AnEvent.
Ocorreu um AnotherEvent.
```

## Mergulho Profundo
Tradicionalmente, em linguagens como Python ou Java, o registro é feito usando uma biblioteca de registro, que fornece uma API direta para registrar mensagens em vários níveis, como debug, info, warning, error e critical.

O Elm, com seu foco em pureza e imutabilidade, não fornece esse tipo de registro direto, pois qualquer tipo de IO ou efeito colateral é gerenciado de forma distinta através da arquitetura do Elm.

Quando você precisa de um registro completo no Elm, normalmente depende de ferramentas externas de JavaScript. As portas, como mostrado acima, são a ponte para essas ferramentas. O módulo Debug é outra opção, mas é destinado apenas para uso durante o desenvolvimento, e não para registro em produção.

Além de portas, os programadores muitas vezes fazem uso das mensagens do compilador do Elm e das facilidades de depuração em tempo de execução, como `Debug.log`, que você pode inserir no seu código para rastrear valores. Ele envolve uma expressão e registra sua saída no console assim:

```Elm
view model =
    Debug.log "Debug do Modelo" model
    -- seu código de visualização aqui
```

No entanto, isso também não é destinado para produção. Ferramentas como elm-logger fornecem algumas abstrações sobre portas para registro, embora estas também sejam destinadas mais para desenvolvimento do que para produção.

## Veja Também
- Portas do Elm: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Diálogo sobre registro no Elm: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Pacote elm-logger: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
