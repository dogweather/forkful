---
title:                "Escrevendo para o Erro Padrão"
html_title:           "Elm: Escrevendo para o Erro Padrão"
simple_title:         "Escrevendo para o Erro Padrão"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador iniciante ou experiente, sabe a importância de escrever códigos limpos e eficientes. Mas o que muitos não sabem é que uma das melhores ferramentas que temos em nossas mãos para garantir isso é o registro de erros. Escrever para o erro padrão pode ajudá-lo a identificar e corrigir rapidamente problemas em seu código.

## Como Fazer

Escrever para o erro padrão em Elm é bastante simples. Você só precisa usar a função `Debug.crash` e passar uma mensagem de erro como argumento. A seguinte linha de código é um exemplo de como isso pode ser feito:

```
Debug.crash "Erro inesperado: x não pode ser igual a y."
```

Quando este código é executado, ele irá parar a execução do programa e imprimir a mensagem de erro no console do navegador. Aqui está a saída que você pode ver no console:

```
Error: Erro inesperado: x não pode ser igual a y.
```

Você também pode usar a função `Debug.log` para escrever para o erro padrão quando estiver depurando seu código. Esta função aceita um valor e uma mensagem de erro como argumentos e irá imprimi-los no console. Aqui está um exemplo de como usá-lo:

```
Debug.log "Variável x" x
```

Isso irá imprimir o valor da variável `x` junto com a mensagem "Variável x" no console do navegador.

## Profundando

Para aqueles que gostam de entender como as coisas funcionam por trás dos panos, é importante saber que, quando usamos as funções `Debug.crash` e `Debug.log`, estamos escrevendo para um tipo de dado chamado `Msg` (mensagem). Este tipo de dado é usado para enviar mensagens simples entre partes do seu programa. Quando você chama `Debug.crash` ou `Debug.log`, está enviando uma mensagem para o log de erros (ou seja, escrevendo para o erro padrão).

Além disso, é importante notar que, ao usar `Debug.crash`, você está interrompendo a execução do programa. Por esse motivo, é recomendável usá-la apenas em situações de erro graves. Use `Debug.log` para mensagens informativas durante o processo de depuração.

## Veja Também

- Documentação oficial sobre registro de erros em Elm: https://guide.elm-lang.org/debugging/errors.html
- Tutorial sobre depuração de código em Elm: https://thoughtbot.com/blog/clearer-errors-with-type-annotations-in-elm
- Exemplo de uso de `Debug.log` em um projeto Elm: https://github.com/zalando/elm-street-view/blob/master/src/Main.elm#L36