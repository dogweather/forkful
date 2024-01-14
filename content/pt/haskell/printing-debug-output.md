---
title:    "Haskell: Imprimindo saída de depuração"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que
Quando estamos programando em Haskell, é comum encontrar erros ou bugs nas nossas funções. Uma maneira eficiente de solucionar esses problemas é utilizar a técnica de impressão de saída de debug. Isso significa adicionar linhas de código especificando valores de variáveis em pontos estratégicos do nosso código para entender melhor o que está acontecendo e onde o erro está ocorrendo.

## Como Fazer
O primeiro passo para adicionar debug output no seu código é importar o módulo Debug.Trace. Isso pode ser feito adicionando a seguinte linha no topo do seu código:
```Haskell
import Debug.Trace
```

Em seguida, podemos utilizar a função "trace" para adicionar a impressão de debug em qualquer lugar do nosso código. Esta função recebe dois argumentos: uma string que será impressa e um valor que será convertido em string e impresso junto com a mensagem. Por exemplo:
```Haskell
func x y = trace ("O valor de x é: " ++ show x) x + y
```

Nesse exemplo, a função "func" irá imprimir o valor de x sempre que for chamada, o que nos ajuda a entender o que está acontecendo dentro dessa função.

## Deep Dive
Além da função "trace", o módulo Debug.Trace oferece outras funções úteis para impressão de debug. A função "traceShow" é semelhante à "trace", mas automaticamente converte o valor passado em string usando a função "show". Já a função "traceStack" imprime a pilha de chamadas quando é chamada, o que pode ser útil para entender a ordem em que as funções estão sendo executadas.

Também é possível combinar a impressão de debug com o uso do comando "if" para condicionalmente imprimir mensagens apenas quando uma determinada condição é verdadeira. Por exemplo:
```Haskell
if x > 10 then trace "x é maior que 10" else trace "x é menor que 10"
```

Outra dica importante é utilizar o módulo Text.Printf para formatar a saída de debug de forma mais legível.

## Veja Também
- [Documentação do módulo Debug.Trace](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Tutorial sobre debugging em Haskell](https://www.schoolofhaskell.com/user/grocid/using-trace-for-debugging)
- [Vídeo explicando o uso de Debug.Trace](https://youtu.be/b1X9FPJYHoE)