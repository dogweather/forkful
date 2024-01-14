---
title:    "Elm: Imprimindo saída de depuração"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que alguém se daria ao trabalho de imprimir mensagens de depuração em um programa Elm. A resposta é simples: imprimir mensagens de depuração é uma técnica útil para entender como seu programa está funcionando e encontrar erros.

## Como Fazer

Para imprimir uma mensagem de depuração em Elm, você pode usar a função `Debug.log` seguida pelo valor que deseja imprimir. Por exemplo:

```Elm
Debug.log "Meu número favorito" 7
```

Isto irá imprimir a mensagem "Meu número favorito" seguida do valor 7 no console do seu navegador. Você também pode imprimir valores de variáveis, como em:

```Elm
let
    meuNome = "Maria"
in
    Debug.log "O nome da pessoa é" meuNome
```

Esta função pode ser particularmente útil quando você está trabalhando com funções e quer verificar se a entrada e saída estão indo como o esperado.

## Profundidade

Mensagens de depuração também podem ser úteis para entender o fluxo de dados em um programa. Você pode imprimir mensagens em diferentes pontos do seu código para ver como os valores estão sendo transformados e utilizados em diferentes partes do programa. Além disso, você pode adicionar mensagens de depuração em diferentes condições para identificar quais partes do seu código estão sendo executadas ou não.

## Veja Também

- Documentação oficial sobre mensagens de depuração em Elm: https://guide.elm-lang.org/debugging/debugging.html
- Tutorial sobre uso de `Debug.log`: https://www.elm-tutorial.org/en/04-starting/06-debugging.html
- Artigo sobre depuração em Elm: https://www.twosixlabs.com/debugging-in-elm/