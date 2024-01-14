---
title:    "TypeScript: Utilizando expressões regulares"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

Expressões regulares são uma importante ferramenta para manipular e buscar padrões de texto em programação. Ao utilizá-las, é possível automatizar tarefas e economizar tempo, além de tornar o código mais eficiente e fácil de entender.

## Como usar expressões regulares em TypeScript

Para utilizar expressões regulares em TypeScript, é necessário primeiro criar uma instância da classe ```RegExp```, que representa uma expressão regular. Dentro dos parênteses, é possível adicionar o padrão que se deseja buscar, utilizando símbolos especiais como ```+```, ```*``` e ```[]```.

Por exemplo, se quisermos buscar todas as palavras que começam com a letra "c", podemos utilizar a expressão regular ```/c\w+/```, onde ```/``` indica o início e o fim da expressão, ```c``` é o padrão que procuramos e ```\w+``` representa qualquer palavra após a letra "c". Podemos então utilizar o método ```test()``` para verificar se uma determinada string possui uma correspondência com a expressão regular, ou o método ```exec()```, que retorna todas as correspondências encontradas.

Veja abaixo um exemplo de como utilizar expressões regulares em TypeScript:

```TypeScript
// Criando uma instância da classe RegExp
const rex = new RegExp(/c\w+/);

// Verificando se uma string possui correspondência
console.log(rex.test("cachorro")); // true
console.log(rex.test("gato")); // false

// Retornando todas as correspondências
console.log(rex.exec("cavalo cavalo cobra")); // ["cavalo", index: 0, input: "cavalo cavalo cobra"]
```

## Mergulho profundo

Existem muitas formas de utilizar expressões regulares em TypeScript, como realizar substituições, buscar padrões específicos dentro de um texto ou até mesmo validar entradas de formulários. Além disso, é possível utilizar flags como ```i```, para ignorar letras maiúsculas e minúsculas, e ```g```, para buscar todas as correspondências em vez de apenas a primeira.

No entanto, é importante ter cuidado ao utilizar expressões regulares, pois podem ser complexas e afetar o desempenho do código. É recomendado sempre testar e verificar se o padrão utilizado retorna os resultados esperados.

## Veja também

- [Documentação oficial do TypeScript sobre expressões regulares](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Expressões regulares: uma introdução para iniciantes](https://medium.com/tableless/express%C3%B5es-regulares-uma-introdu%C3%A7%C3%A3o-para-iniciantes-33a1ee687182)
- [Tutorial de expressões regulares em TypeScript](https://www.tutorialspoint.com/typescript/typescript_regex.htm)