---
title:                "Buscando e substituindo texto"
html_title:           "TypeScript: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que 

Realizar a busca e substituição de textos é uma tarefa comum em programação. Essa prática permite que sejam feitas alterações em massa em um grande volume de dados de forma rápida e eficiente.

## Como fazer 

Para realizar a busca e substituição de texto em TypeScript, é necessário utilizar a função `replace()`. Essa função recebe dois parâmetros: o primeiro é o texto a ser buscado e o segundo é o novo texto que irá substituí-lo. Veja um exemplo prático abaixo:

```TypeScript
let str: string = "Olá mundo!";

// Substituindo "mundo" por "Brasil"
let novoStr = str.replace("mundo", "Brasil");

console.log(novoStr); // Saída: Olá Brasil!
```

No exemplo acima, a função `replace()` é usada para substituir a palavra "mundo" por "Brasil" na string inicial. É importante ressaltar que a função `replace()` substitui apenas a primeira ocorrência do texto a ser buscado. Para substituir todas as ocorrências, é necessário utilizar uma expressão regular. Veja o exemplo abaixo:

```TypeScript
let str: string = "Ser ou não ser, eis a questão.";

// Substituindo todas as ocorrências de "ser" por "estar"
let novoStr = str.replace(/ser/g, "estar");

console.log(novoStr); // Saída: Estar ou não estar, eis a questão.
```

## Mergulho Profundo 

Além da função `replace()`, TypeScript também possui outras formas de realizar a busca e substituição de textos. Uma delas é utilizando a função `replaceAll()`, que substitui todas as ocorrências do texto a ser buscado. Dessa forma, não é necessário utilizar a expressão regular como no exemplo anterior.

Outra opção é utilizar o método `split()` em conjunto com o método `join()`. O método `split()` separa a string em um array, utilizando o texto a ser buscado como delimitador. Já o método `join()` une os elementos do array utilizando o novo texto como separador. Veja o exemplo abaixo:

```TypeScript
let str: string = "Dia da árvore";

// Substituindo "árvore" por "natureza"
let novoStr = str.split("árvore").join("natureza");

console.log(novoStr); // Saída: Dia da natureza
```

Esses são apenas alguns exemplos de como realizar a busca e substituição de texto em TypeScript. Existem diversas outras formas de fazer essa tarefa, e cabe ao programador escolher qual é a melhor opção para cada situação.

## Veja também 

- Documentação completa da função `replace()` no site oficial do TypeScript: https://www.typescriptlang.org/docs/handbook/strings.html#replace
- Mais informações sobre expressões regulares em TypeScript: https://www.sitepoint.com/regular-expressions-typescript/
- Tutorial de busca e substituição de texto em TypeScript: https://codecraft.tv/courses/typescript/regular-expressions-and-string-methods/search-replace/