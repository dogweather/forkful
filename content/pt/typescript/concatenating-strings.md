---
title:                "Concatenando strings"
html_title:           "TypeScript: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, quando estamos criando um programa, precisamos combinar diferentes pedaços de texto em uma única string. Isso pode ser feito de várias maneiras, mas a concatenação de strings é a mais comum e eficiente. 

## Como fazer

Para concatenar strings no TypeScript, podemos usar o operador "+" ou a template literals. Aqui está um exemplo usando o operador "+":

```TypeScript 
let nome = "João";
let sobrenome = "da Silva";
let nomeCompleto = nome + " " + sobrenome;
console.log(nomeCompleto); // Saída: "João da Silva"
```

Também podemos usar template literals para concatenar strings. Isso nos permite inserir valores de variáveis diretamente na string, sem a necessidade de usar o operador "+". Veja um exemplo:

```TypeScript 
let idade = 25;
let frase = `Eu tenho ${idade} anos.`;
console.log(frase); // Saída: "Eu tenho 25 anos."
```

## Aprofundando

Ao usar o operador "+", o TypeScript converte automaticamente outros tipos de dados em strings antes de concatená-los. Por exemplo:

```TypeScript
let num = 10;
let texto = "O número é " + num;
console.log(texto); // Saída: "O número é 10"
```

Podemos também utilizar o método `concat()` para concatenar strings em vez do operador "+". O método `concat()` recebe um ou mais argumentos e os une em uma única string. 

```TypeScript
let texto1 = "Welcome ";
let texto2 = "to TypeScript";
console.log(texto1.concat(texto2)); // Saída: "Welcome to TypeScript"
```

## Veja também

- [Documentação oficial do TypeScript sobre strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Artigo sobre templates literals em TypeScript](https://blog.logrocket.com/template-literals-typescript-f3c6bc2b72bc)
- [Mais sobre o método concat()](https://www.w3schools.com/jsref/jsref_concat_string.asp)