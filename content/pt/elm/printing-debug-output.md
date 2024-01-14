---
title:                "Elm: Impressão de saída de depuração"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de debug?

Imprimir saída de debug é uma ferramenta útil para entender melhor o fluxo e a lógica do seu código. Ele permite que você visualize informações importantes em tempo real e encontre possíveis erros no seu programa.

## Como fazer isso em Elm?

A linguagem de programação Elm tem uma função incorporada chamada `Debug.log` que permite que você imprima mensagens de debug no console do navegador. Aqui está um exemplo de como usá-lo:

```Elm
x = 5
y = 10
soma = x + y

Debug.log "Soma" soma
```

Na saída do console do navegador, você verá a mensagem "Soma: 15". Isso pode ser útil para verificar se os valores das variáveis estão sendo atribuídos corretamente ou para entender melhor como o seu código está funcionando em diferentes cenários.

## Profundando mais na impressão de saída de debug

Além de simplesmente imprimir mensagens, você também pode usar a função `Debug.log` para visualizar informações sobre outras expressões, como listas ou registros. Por exemplo:

```Elm
lista = [1, 2, 3, 4]

Debug.log "Lista" lista

pessoa = { nome = "Maria", idade = 30 }

Debug.log "Pessoa" pessoa
```

Na saída do console do navegador, você verá a lista completa `[1,2,3,4]` e informações mais detalhadas sobre a estrutura do registro `{ nome = "Maria", idade = 30 }`. Isso pode ajudá-lo a depurar e entender melhor suas estruturas de dados.

## Veja também

- Documentação oficial Elm sobre a função `Debug.log`: http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#log
- Guia para depuração em Elm: https://digitalfortress.tech/elm-debugging/
- Vídeo tutorial sobre como imprimir saída de debug em Elm: https://www.youtube.com/watch?v=Y3mFHMg5A6k