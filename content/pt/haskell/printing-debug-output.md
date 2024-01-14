---
title:                "Haskell: Imprimindo saída de depuração"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, quando estamos programando em Haskell, encontramos bugs e erros em nosso código. E uma das formas mais eficazes de solucioná-los é utilizando a impressão de saída de depuração. Isso nos permite visualizar o estado do nosso programa em diferentes pontos de execução, facilitando a identificação e correção dos problemas.

## Como fazer

Para imprimir saída de depuração em Haskell, podemos usar a função `Debug.Trace.trace` e passar como argumento a mensagem que desejamos exibir. Por exemplo:

```Haskell
import Debug.Trace

main = do
    let x = 5
    trace ("Valor de x: " ++ show x) -- exibe a mensagem e retorna o valor de x
    let y = 10 + x
    trace ("Valor de y: " ++ show y)
    putStrLn "Fim do programa"
```

A saída desse código será:

```
Valor de x: 5
Valor de y: 15
Fim do programa
```

Podemos também utilizar a função `Debug.Trace.traceShow` para exibir o valor de uma variável junto com uma mensagem:

```Haskell
import Debug.Trace

main = do
    let x = 5
    let y = 10 + x
    traceShow (x, y) "Valores de x e y:" -- exibe a mensagem e os valores de x e y
    putStrLn "Fim do programa"
```

A saída será:

```
Valores de x e y: (5,15)
Fim do programa
```

## Deep Dive

Além de imprimir mensagens de depuração, também podemos utilizar a função `Debug.Trace.traceStack` para exibir a pilha de chamadas do programa. Isso é especialmente útil quando estamos lidando com funções recursivas, pois nos ajuda a entender melhor o fluxo de execução.

Outra dica importante é utilizar a diretiva de compilação `-DDEBUG` para habilitar a impressão de saída de depuração apenas quando estamos em desenvolvimento e não em produção. Isso evita que nossa aplicação gere mensagens desnecessárias em ambientes de produção.

## Veja também

- [Documentação do módulo `Debug.Trace` em Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Tutorial de depuração em Haskell](https://wiki.haskell.org/Debugging)
- [Exemplo prático de depuração em Haskell](https://tutorialspoint.dev/language/haskell/debugging-in-haskell)

Espero que este artigo tenha sido útil para entender a importância e como utilizar a impressão de saída de depuração em Haskell. Com essas ferramentas em mãos, podemos tornar o processo de debug mais eficiente e eficaz. Se tiver alguma dúvida, fique à vontade para deixar um comentário abaixo. Happy debugging! ;)