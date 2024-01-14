---
title:                "Elm: Escrevendo para o erro padrão"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão? 

Se você é um programador iniciante ou experiente, já deve ter se deparado com o desafio de encontrar erros em seu código. O Elm oferece uma ótima maneira de lidar com esse problema, permitindo que você escreva para o erro padrão. Mas por que exatamente você deveria fazer isso? Neste post, vamos explorar as vantagens e como fazer isso.

## Como fazer 

Escrever para o erro padrão é um processo simples em Elm. Tudo que você precisa fazer é usar a função `Debug.log` e fornecer uma string como primeiro argumento e qualquer valor como segundo argumento. Por exemplo:

```elm
meuDado = "olá"
Debug.log "mensagem" meuDado
```

Isso vai imprimir "mensagem: olá" no seu console. Você também pode usar essa função para depuração, como por exemplo:

```elm
isEven num =
  Debug.log "checando paridade" (num % 2 == 0)
```

Isso vai imprimir "checando paridade: True" ou "checando paridade: False" dependendo do valor de `num`. Assim, você pode verificar se sua função está funcionando corretamente em tempo real.

## Mergulho profundo 

Além de ser útil para depuração, escrever para o erro padrão também pode ser útil para entender o fluxo de dados em sua aplicação. Por exemplo, você pode usar a função `Debug.log` para imprimir valores em diferentes pontos do seu código e ver como eles mudam conforme sua aplicação é executada. Isso pode ajudar a identificar onde os erros estão ocorrendo e a entender melhor o comportamento do seu código.

Outra vantagem de escrever para o erro padrão é que isso não afeta o desempenho da sua aplicação em produção. O compilador do Elm remove automaticamente essas chamadas de depuração do código gerado, garantindo que sua aplicação funcione da melhor forma possível.

## Veja também 

Aqui estão alguns links para mais recursos sobre escrever para o erro padrão em Elm:

- Documentação oficial: https://guide.elm-lang.org/debugging/debugging.html
- Artigo sobre depuração em Elm: https://dev.to/sleeptillseven/debugging-in-elm-2flo
- Vídeo explicando a função `Debug.log`: https://youtu.be/ruvExM-JdM4 

Agora que você sabe como e por que escrever para o erro padrão em Elm, experimente em suas próprias aplicações e veja como pode facilitar seu processo de desenvolvimento. Lembre-se sempre de remover essas chamadas de depuração do seu código antes de publicá-lo em produção.