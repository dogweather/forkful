---
title:    "TypeScript: Concatenação de strings"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

Por que utilizar a concatenação de strings em TypeScript?

A concatenação de strings é uma tarefa comum em programação, especialmente quando se lida com strings de texto longas ou complexas. Em TypeScript, essa técnica é especialmente útil para criar mensagens personalizadas ou para combinar dados de diferentes variáveis em uma única string.

## Como fazer

Para realizar uma concatenação de strings em TypeScript, utilizamos o operador "+" ou a função "concat()". Vamos ver alguns exemplos abaixo:

```TypeScript
let nome = "João";
let sobrenome = "Silva";

console.log(nome + " " + sobrenome);
// output: João Silva

let idade = 25;

console.log("Olá, eu sou o " + nome + ", tenho " + idade + " anos.");
// output: Olá, eu sou o João, tenho 25 anos.

console.log(nome.concat(" ", sobrenome));
// output: João Silva
```

## Deep Dive

Além dos exemplos mostrados acima, é importante lembrar que a concatenação de strings em TypeScript pode ser realizada com diferentes tipos de dados, não apenas com strings. Isso significa que podemos combinar uma string com um número, um booleano, entre outros.

```TypeScript
let ano = 2021;
let mensagem = "Estamos no ano de ";

console.log(mensagem + ano);
// output: Estamos no ano de 2021
```

Outra dica útil é utilizar os templates literais para realizar concatenações de strings de forma mais prática e legível. Eles permitem a interpolação de variáveis diretamente na string, usando o símbolo "$" e colocando a variável dentro de chaves.

```TypeScript
let produto = "lápis";
let quantidade = 5;

console.log(`Eu comprei ${quantidade} ${produto}s.`);
// output: Eu comprei 5 lápis.
```

## Veja também

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Guia completo de strings em TypeScript](https://www.tutorialsteacher.com/typescript/string)
- [Outros operadores úteis em TypeScript](https://www.freecodecamp.org/news/the-typescript-handbook-operators/)