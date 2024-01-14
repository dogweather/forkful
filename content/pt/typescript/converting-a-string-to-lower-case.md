---
title:    "TypeScript: Convertendo uma string para minúsculas"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para letras minúsculas?

Converter uma string para letras minúsculas é uma tarefa comum em programação para garantir que a entrada do usuário seja padronizada e fácil de manipular. Também pode ser útil ao comparar strings, pois letras maiúsculas e minúsculas são tratadas de forma diferente em muitas linguagens de programação.

## Como fazer

Para converter uma string em letras minúsculas em TypeScript, podemos utilizar o método `toLowerCase()`.

Exemplo 1:
```
TypeScript
let str: string = "EXEMPLO";
console.log(str.toLowerCase());
// Output: exemplo
```
Neste exemplo, declaramos uma variável `str` com o valor "EXEMPLO" e, em seguida, usamos o método `toLowerCase()` para converter a string em letras minúsculas. O retorno é o valor "exemplo".

Se quisermos converter uma string em letras minúsculas, mas manter o primeiro caractere maiúsculo, podemos usar a função `charAt()` para extrair o primeiro caractere, aplicar o método `toLowerCase()` e, em seguida, concatenar novamente com o restante da string.

Exemplo 2:
```
TypeScript
function capitalize(str: string) {
    return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
}

console.log(capitalize("exeMplo"));
// Output: Exemplo
```
Neste exemplo, criamos uma função `capitalize` que utiliza o método `charAt()` para extrair o primeiro caractere e aplicar o método `toUpperCase()` para torná-lo maiúsculo. Em seguida, usamos o método `slice()` para obter o restante da string e aplicar o método `toLowerCase()` para tornar todas as outras letras minúsculas. Por fim, concatenamos o primeiro caractere maiúsculo com o restante da string em letras minúsculas.

## Deep Dive

Ao converter uma string em letras minúsculas, devemos ter em mente que nem todos os caracteres serão convertidos. Em muitas linguagens de programação, apenas letras alfabéticas serão convertidas, caracteres especiais e números permanecerão inalterados.

Além disso, devemos considerar o idioma da string. Algumas linguagens têm caracteres especiais acentuados, que serão convertidos de forma diferente para letras minúsculas. Por exemplo, em português, o caractere "Ç" é comumente usado e deve ser convertido para "ç" em letras minúsculas.

## Veja também

- [Referência do método `toLowerCase()` em MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Uso de strings em TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Manipulando strings com TypeScript](https://www.digitalocean.com/community/tutorials/typescript-string-manipulation-pt)