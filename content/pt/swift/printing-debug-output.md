---
title:    "Swift: Imprimindo saída de depuração"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que

Ao escrever um código em Swift, é comum encontrar erros ou bugs que afetam a funcionalidade do seu aplicativo. Ao imprimir informações de depuração (debug output), você pode ter uma visão mais clara do que está acontecendo no seu código e assim, facilitar a identificação e correção desses erros.

## Como fazer

Para imprimir informações de depuração em Swift, você pode utilizar a função "print()" seguida dos valores ou variáveis que deseja imprimir. Por exemplo:

```
let numero = 5
print("O número é: \(numero)")
```

A saída para este código seria: "O número é: 5", mostrando que a variável "numero" tem o valor 5. É importante notar que você também pode imprimir textos e variáveis juntos, utilizando o operador de concatenação ("\( )").

## Toques avançados

Existem algumas maneiras mais avançadas de imprimir informações de depuração em Swift. Uma delas é utilizar a função "dump()" que permite imprimir a estrutura completa de um objeto ou variável. Por exemplo:

```
dump(numeros)
```

A saída para este código seria algo similar a:

```
▿ 5 elementos
- 1 : 2
- 2 : 5
- 3 : 7
- 4 : 9
- 5 : 10
```

Esta função é útil quando você precisa ver todos os valores de uma lista, dicionário ou outro objeto complexo.

## Veja também

- Veja mais sobre a função `print()` na documentação oficial da Apple: https://developer.apple.com/documentation/swift/stream/1407783-print
- Para saber mais sobre a função `dump()`, acesse: https://developer.apple.com/documentation/swift/debug/2884813-dump