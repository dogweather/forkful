---
title:    "TypeScript: Encontrando o comprimento de uma string"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string em TypeScript?

Encontrar o comprimento de uma string é uma tarefa comum em programação, independentemente da linguagem utilizada. Isso pode ser útil para validar dados de entrada, formatar saídas ou realizar outras operações. Neste artigo, vamos discutir como encontrar o comprimento de uma string em TypeScript.

## Como fazer

Existem duas maneiras de encontrar o comprimento de uma string em TypeScript: o método `length` e o operador `?.`.

### Usando o método `length`

O método `length` é um método nativo da classe `String` em TypeScript. Este método retorna o tamanho da string em caracteres.

```TypeScript
const minhaString = "Olá mundo!"
console.log(minhaString.length) // output: 11
```

### Usando o operador `?.`

O operador `?.` é uma novidade do TypeScript 3.7. Ele pode ser usado para verificar se uma propriedade existe antes de acessá-la. Ao usá-lo com uma string, ele retorna o comprimento da mesma.

```TypeScript
const minhaString = "Olá mundo!"
console.log(minhaString?.length) // output: 11
```

## Explorando mais a fundo

O comprimento de uma string em TypeScript é determinado pelo número de caracteres que ela possui, incluindo espaços e caracteres especiais. Além disso, o método `length` é sensível a maiúsculas e minúsculas, ou seja, ele contará letras maiúsculas e minúsculas como caracteres diferentes.

## Veja também
- [Documentação sobre o método `length` em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#optional-chaining)
- [Explicação sobre o operador `?.` em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-7.html#optional-chaining)
- [Exemplos práticos de encontrar o comprimento de uma string em TypeScript](https://www.educative.io/edpresso/how-to-find-string-length-in-typescript)