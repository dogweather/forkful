---
title:                "TypeScript: Encontrando o comprimento de uma string"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Porque

Aprender a encontrar o comprimento de uma string é uma tarefa importante em programação TypeScript. Com essa habilidade, você será capaz de manipular e analisar strings de maneira mais eficaz, tornando seus códigos mais eficientes e elegantes.

## Como fazer

Encontrar o comprimento de uma string pode parecer simples, mas existem várias maneiras de fazer isso em TypeScript. Aqui estão três exemplos de como fazer isso:

```
TypeScript
// Usando o método integrado 'length'
let minhaString = "Olá, mundo!";
console.log(minhaString.length); // Saída: 12

// Usando uma função para contar os caracteres
function encontrarComprimento(texto: string): number {
  let contador = 0;
  for (let i = 0; i < texto.length; i++) {
    contador++;
  }
  return contador;
}
console.log(encontrarComprimento(minhaString)); // Saída: 12

// Usando uma função recursiva
function encontrarComprimentoRecursivo(texto: string): number {
  if (texto === "") {
    return 0;
  } else {
    return 1 + encontrarComprimentoRecursivo(texto.substring(1));
  }
}
console.log(encontrarComprimentoRecursivo(minhaString)); // Saída: 12
```

## Mergulho Profundo

Uma string é uma coleção de caracteres, portanto, encontrar seu comprimento significa contar quantos caracteres ela contém. O método 'length' é o mais simples e mais utilizado para encontrar o comprimento de uma string em TypeScript. Ele retorna o número total de caracteres, incluindo espaços.

A opção de utilizar uma função para contar os caracteres permite uma maior flexibilidade e personalização em relação ao que deve ser contado. Nesse exemplo, utilizamos um loop for para percorrer cada caractere da string e incrementar um contador a cada iteração.

Já a função recursiva utiliza um conceito chamado "divisão e conquista", em que a string é dividida em partes menores e o comprimento de cada parte é somado até se chegar ao comprimento total. Isso pode ser útil quando se trabalha com strings mais complexas.

## Veja também

- [Documentação oficial do método 'length'](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Explicação sobre funções recursivas em TypeScript](https://www.typescriptlang.org/docs/handbook/functions.html#recursive-functions)
- [Exemplos de manipulação de strings em TypeScript](https://www.java67.com/2016/01/typescript-string-examples.html)