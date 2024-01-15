---
title:                "Imprimindo saída de depuração"
html_title:           "TypeScript: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que 

Muitas vezes, durante o processo de desenvolvimento de software, nos deparamos com situações em que precisamos entender o que está acontecendo em nosso código em tempo real. Isso pode incluir encontrar bugs, monitorar o fluxo de dados ou entender o funcionamento de um novo recurso. É aí que entra a impressão de saída de depuração (debug output). É uma ferramenta útil para facilitar o processo de desenvolvimento e garantir que o código funcione corretamente.

## Como fazer

Para imprimir saída de depuração em TypeScript, podemos usar o método console.log(). Este método recebe como parâmetro um ou vários valores e os imprime no console do seu navegador, node.js ou terminal. Vamos dar uma olhada em alguns exemplos:

```TypeScript
console.log("Olá, mundo!");

// saída: Olá, mundo!
```

```TypeScript
let num1 = 10;
let num2 = 20;

console.log(num1 + num2);

// saída: 30
```

Podemos até mesmo imprimir objetos e seus valores para nos ajudar a entender melhor seu conteúdo:

```TypeScript
let pessoa = {
  nome: "João",
  idade: 25,
  profissao: "Engenheiro"
};

console.log(pessoa);

/* saída:
{
  nome: 'João',
  idade: 25,
  profissao: 'Engenheiro'
} 
*/
```

## Deep Dive 

Outra forma de imprimir saída de depuração é utilizando o módulo de depuração do TypeScript. Ele possui um método chamado `debug()`, que pode ser usado para imprimir valores específicos durante a execução do código. Vamos ver um exemplo de como usar esse método:

```TypeScript
import * as depuracao from "depuracao";

let cor = "azul";
let valor = 50;

depuracao.debug("A cor é", cor);
depuracao.debug("O valor é", valor);

// saída: A cor é azul
// saída: O valor é 50
```

Usando o módulo de depuração, podemos até mesmo adicionar cores e formatação à nossa saída de depuração, tornando-a mais visualmente agradável e fácil de entender.

## Veja também 

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs)
- [Módulo de depuração do TypeScript](https://www.npmjs.com/package/debug)