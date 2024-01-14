---
title:    "TypeScript: Imprimir saída de depuração"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante

Imprimir saída de depuração é uma prática importante na programação porque permite que os desenvolvedores visualizem informações fundamentais durante o processo de desenvolvimento. Isso pode ser especialmente útil para encontrar e corrigir erros em um código.

## Como imprimir saída de depuração em TypeScript

Para imprimir saída de depuração em TypeScript, podemos usar a função `console.log()`. Essa função aceita qualquer tipo de variável como argumento e imprime seu conteúdo no console do navegador ou do ambiente de execução. Aqui está um exemplo:

```TypeScript
let nome: string = "Maria";
let idade: number = 25;

console.log(nome); // Output: Maria
console.log("A idade de " + nome + " é " + idade + " anos."); // Output: A idade de Maria é 25 anos.
```

## Aprofundando na saída de depuração

Existem outras formas de imprimir saída de depuração em TypeScript, como `console.debug()`, `console.info()`, `console.warn()` e `console.error()`. Cada uma delas tem um uso específico e pode ser útil em diferentes situações. É importante lembrar de remover essas linhas de código antes de implantar o projeto, pois elas podem afetar o desempenho da aplicação.

Além disso, é possível formatar a saída de depuração usando as `string templates` do TypeScript. Por exemplo:

```TypeScript
let a: number = 5;
let b: number = 3;

console.log(`A soma de ${a} + ${b} é igual a ${a+b}.`); // Output: A soma de 5 + 3 é igual a 8.
```

## Veja também

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Desenvolvimento web com TypeScript](https://www.devmedia.com.br/desenvolvimento-web-com-typescript/36644)
- [Tutorial do TypeScript para iniciantes](https://www.tuneup.tech/blog/typescript--o-que-e-e-como-usar)