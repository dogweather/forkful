---
title:    "TypeScript: Buscando e substituindo texto"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Há momentos em que precisamos fazer uma alteração em um texto extenso, seja corrigindo erros ortográficos ou substituindo uma palavra por outra. Em vez de fazer essas alterações manualmente, a busca e substituição de texto pode ser uma maneira mais rápida e eficiente de fazer essas alterações.

## Como

Para realizar a busca e substituição de texto em TypeScript, podemos usar a função `replace()` da classe `String`. Veja um exemplo de código abaixo:

```TypeScript
let texto = "Este é um exemplo de busca e substituição de texto em TypeScript.";
let novoTexto = texto.replace("exemplo", "demo");

console.log(novoTexto);
```

A saída deste código será: "Este é um demo de busca e substituição de texto em TypeScript."

Podemos também usar expressões regulares com a função `replace()` para substituir múltiplas ocorrências de uma palavra. Por exemplo:

```TypeScript
let texto = "Este é um exemplo de busca e substituição de texto em TypeScript.";
let novoTexto = texto.replace(/exemplo/g, "demo");

console.log(novoTexto);
```

A saída deste código será: "Este é um demo de busca e substituição de texto em TypeScript."

## Deep Dive

Para entender melhor como a função `replace()` funciona, é importante entender que ela recebe dois parâmetros: o primeiro é o texto que será substituído e o segundo é o texto que será inserido no lugar.

No primeiro exemplo de código, usamos diretamente uma string como primeiro parâmetro. No entanto, também podemos usar uma expressão regular, como mostrado no segundo exemplo, para substituir múltiplas ocorrências de um texto.

Além disso, a função `replace()` também pode receber uma função como segundo parâmetro, permitindo que um código personalizado seja executado para cada correspondência encontrada.

## Veja também

- [Documentação oficial do TypeScript sobre a função `replace()`](https://www.typescriptlang.org/docs/handbook/declaration-merging.html#declaring-a-merge-on-a-function)
- [Artigo sobre expressões regulares em TypeScript](https://medium.com/@jeronimo_28061/expresiones-regulares-en-typescript-4cabdf10a196)
- [Guia de referência de expressões regulares em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)