---
title:                "Usando um depurador"
date:                  2024-01-26T03:48:51.660544-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-a-debugger.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Depurar no Elm envolve identificar e remover erros do seu código. Programadores fazem isso para garantir que suas aplicações funcionem corretamente e para melhorar a qualidade do código. O forte sistema de tipos do Elm captura muitos problemas em tempo de compilação, mas as ferramentas de depuração em tempo de execução são essenciais para resolver erros de lógica e comportamentos inesperados.

## Como fazer:
O Elm não possui um depurador integrado no sentido tradicional, como, por exemplo, o JavaScript tem com as ferramentas de desenvolvedor do navegador. No entanto, a comunidade Elm desenvolveu ferramentas para preencher essa lacuna. Aqui está como você pode usar o `elm-debug-transformer` para depurar seu aplicativo Elm:

```Elm
-- Instalar elm-debug-transformer (pacote Node)

1. npm install -g elm-debug-transformer

-- Use elm-debug-transformer para iniciar seu aplicativo

2. elm-debug-transformer --port=8000 yourMainElmFile.elm
```

Uma vez que o `elm-debug-transformer` está em execução, ele cria uma conexão WebSocket para registro de logs. Você verá informações de depuração no console do seu navegador, onde pode inspecionar as estruturas de dados do seu programa em pontos dados da sua aplicação.

No Elm 0.19 e posteriores, as funções do módulo `Debug` como `Debug.log` e `Debug.todo` podem ajudá-lo a rastrear valores e marcar deliberadamente partes inacabadas do seu código. Veja como usar o Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Incrementando" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Decrementando" { model | count = model.count - 1 }, Cmd.none )
```

Você verá mensagens de "Incrementando" ou "Decrementando" no console do seu navegador junto com o novo estado do `model`.

## Aprofundamento
O autor do Elm, Evan Czaplicki, teve como objetivo criar uma linguagem onde bugs comuns fossem impossíveis ou fáceis de detectar. Essa filosofia é o motivo pelo qual o núcleo do Elm não inclui funções de depuração tradicionais. A análise estática e a inferência de tipo do Elm contribuem massivamente para reduzir erros em tempo de execução, o que diminui a necessidade de depuração sofisticada em tempo de execução. Alternativas históricas incluíam o uso do agora obsoleto `elm-reactor`, que oferecia depuração com viagem no tempo – uma maneira de rebobinar e reproduzir ações no seu aplicativo.

Hoje, ferramentas como o `elm-debug-transformer` e o uso do módulo `Debug` do Elm ajudam a preencher essa lacuna. Enquanto o módulo `Debug` é destinado para uso durante o desenvolvimento apenas e deve ser removido antes de compilações para produção, ele é uma ferramenta inestimável para identificar e registrar mudanças de estado.

Tenha em mente que técnicas tradicionais de depuração em JavaScript, como pontos de interrupção ou execução passo a passo, não são diretamente aplicáveis no Elm devido à sua arquitetura e como o runtime do Elm lida com atualizações de estado. O Elm encoraja você a estruturar seu programa de forma que o fluxo de dados seja claro e siga garantias estritas de tipos e imutabilidade, minimizando os casos em que a depuração é necessária.

## Veja Também
- Guia oficial do Elm sobre tratamento de exceções em tempo de execução: https://guide.elm-lang.org/error_handling/
- Repositório GitHub do `elm-debug-transformer`: https://github.com/kraklin/elm-debug-transformer
- Tópico no fórum do Elm discutindo estratégias de depuração: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Documentação do módulo `Debug` do Elm: https://package.elm-lang.org/packages/elm/core/latest/Debug