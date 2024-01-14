---
title:    "TypeScript: Excluindo caracteres que correspondem a um padrão"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Porquê

Há muitas razões pelas quais alguém pode querer deletar caracteres que coincidam com um padrão em um código TypeScript. Por exemplo, pode ser necessário limpar dados ou formatar strings em um formato específico. Independentemente do motivo, poder deletar caracteres que correspondam a um determinado padrão é uma habilidade valiosa para qualquer desenvolvedor TypeScript.

## Como Fazer

Para deletar caracteres que correspondam a um padrão em um código TypeScript, vamos usar o método `replace()` da classe `String`. Este método substitui parte da string por uma nova string, com base em um padrão fornecido.

Aqui está um exemplo de código que usa o método `replace()` para deletar vogais de uma string:

```TypeScript
let str: string = "Olá mundo!";
str = str.replace(/[aeiou]/ig, "");
console.log(str); // Output: "Ol mnd!"
```

Neste exemplo, usamos uma expressão regular para identificar as vogais na string e então substituímos esses caracteres por uma string vazia (ou seja, deletando-os).

## Deep Dive

O método `replace()` pode ser usado de várias maneiras para manipular uma string e deletar caracteres que correspondam a um padrão. Alguns exemplos incluem a substituição de caracteres por outros caracteres, a adição de novos caracteres em uma determinada posição e a substituição por strings que correspondam a um padrão.

Além disso, é importante observar que o método `replace()` substitui apenas a primeira ocorrência do padrão. Se você quiser substituir todas as ocorrências, é necessário adicionar a flag `g` (para substituir globalmente) à expressão regular. A flag `i` também pode ser usada para ignorar as maiúsculas e minúsculas durante a comparação.

## Veja Também

- [Documentação do método `replace()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Expressões Regulares em TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutorial de Expressões Regulares em MDN](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)