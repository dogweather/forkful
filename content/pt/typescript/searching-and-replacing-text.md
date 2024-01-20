---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Buscar e Substituir Texto em TypeScript

## O Quê & Porquê?

Buscar e substituir texto é uma operação comum que permite localizar uma sequência de texto dentro de outra e substituí-la por uma nova sequência. Programadores fazem isso para manipular dados, corrigir erros e melhorar a legibilidade do código.

## Como Fazer:

Em TypeScript, você pode utilizar o método `replace()` para buscar e substituir texto. Abaixo está um exemplo:

```TypeScript
let texto = "Olá, Mundo!";
let novoTexto = texto.replace("Mundo", "TypeScript");
console.log(novoTexto);  // Saída: "Olá, TypeScript!"
```

No código acima, a função `replace()` procura a string "Mundo" e a substitui por "TypeScript".

## Aprofundando

Historicamente, a necessidade de buscar e substituir texto remonta aos primeiros dias da computação. Selecione a ferramenta adequada com base nas necessidades do seu projeto. Além da função `replace()`, existem outras maneiras de procurar e substituir texto em TypeScript, como a expressão regular.

```TypeScript
let texto = "Olá, Mundo!";
let novoTexto = texto.replace(/Mundo/g, "TypeScript");
console.log(novoTexto);  // Saída: "Olá, TypeScript!"
```

No código acima, `/Mundo/g` é uma expressão regular que busca por todas as ocorrências de "Mundo" no texto e as substitui por "TypeScript". O 'g' significa 'global', o que significa que procurará em todo o texto, não apenas na primeira ocorrência.

## Veja Também

Para obter mais informações sobre as operações de string em TypeScript, confira estes links:

1. Documentação oficial do TypeScript sobre [Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
2. Uma introdução ao [TypeScript com exemplos](https://www.tutorialsteacher.com/typescript)
3. Documentação oficial do JavaScript na MDN sobre [replace()](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/Replace) e [Expressões regulares](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions).