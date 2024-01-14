---
title:                "TypeScript: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que
Aprender a encontrar o comprimento de uma string é essencial para entender a manipulação básica de dados em TypeScript. Saber como determinar o tamanho de uma string é uma habilidade importante para qualquer programador TypeScript.

## Como Fazer
Para encontrar o comprimento de uma string em TypeScript, podemos utilizar a propriedade `length` da string. Vamos dar uma olhada em um exemplo simples:
```TypeScript
let nome = "Maria";
console.log(nome.length);
```
Este código irá imprimir o número 5 no console, indicando que o comprimento da string "Maria" é de 5 caracteres.

Além disso, podemos usar o método `charAt()` para retornar o caractere em uma posição específica na string. Por exemplo:
```TypeScript
let frase = "Olá mundo!";
console.log(frase.charAt(0));
```
O código acima irá imprimir "O" no console, pois a letra "O" está na posição 0 da string.

Podemos até mesmo combinar o uso do método `charAt()` com a propriedade `length` para acessar o último caractere de uma string:
```TypeScript
console.log(frase.charAt(frase.length - 1));
```
Este código irá imprimir "!" no console, pois a posição do último caractere em "Olá mundo!" é 12 (sendo 11 o caractere "o" e 12 o caractere "!").

## Mergulho Profundo
Entender o conceito de codificação Unicode é fundamental para entender como o comprimento de uma string é determinado em TypeScript. Basicamente, cada caractere em uma string é representado por um código numérico. A propriedade `length` retorna o número de caracteres de uma string, mas nem sempre corresponde ao número de letras visíveis.

Por exemplo, a palavra "é" pode ser representada por dois caracteres em Unicode: "e" e " ́", que juntos formam o caractere "é". No entanto, a propriedade `length` irá retornar 1 para a string "é".

## Veja Também
- [Documentação oficial do TypeScript sobre strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Tutorial de TypeScript do MDN](https://developer.mozilla.org/en-US/docs/Learn/Tools_and_testing/Client-side_JavaScript_frameworks/TypeScript_tutorial)
- [Artigo sobre codificação Unicode](https://en.wikipedia.org/wiki/Unicode)