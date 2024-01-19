---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
A exclusão de caracteres que correspondem a um padrão é uma tarefa comum no desenvolvimento de JavaScript. É usada quando se deseja limpar, formatar ou manipular strings de acordo com critérios específicos.

## Como Fazer:

Para excluir caracteres que correspondem a um padrão específico, usamos o método `replace()` com uma expressão regular. Aqui está um exemplo de como você pode fazer isso:

```Javascript
let stringOriginal = "Ola, mundo!";
let padrao = /Ola/g;
let novaString = stringOriginal.replace(padrao, "");

console.log(novaString);
// Saída: ", mundo!"
```
Neste exemplo, removemos todas as ocorrências do padrão 'Ola' da string original.

## Mergulhando Mais Fundo

O método `replace()` foi introduzido já na primeira versão do JavaScript, ECMAScript 1, em 1997. Expressões regulares (regex) foram ainda mais antigas e têm raízes na linguagem de programação Perl dos anos 80.

Existem alternativas ao `replace()`, como o uso do método `split()` em combinação com `join()`. No entanto, `replace()` torna as coisas muito mais simples e legíveis.

Os detalhes de implementação da exclusão de caracteres com um padrão dependem muito do padrão e das necessidades específicas do seu programa. Por exemplo, você pode querer combinar o `replace()` com outros métodos de string, como `trim()` para remover espaços em branco desnecessários.

## Veja Também

Para aprender mais sobre o método `replace()` e expressões regulares em JavaScript, confira esses links:

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: Expressões regulares](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [JavaScript.info: Expressões regulares](https://javascript.info/regular-expressions)