---
title:                "TypeScript: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer a substituição de texto em TypeScript?

A busca e substituição de texto é uma tarefa comum em muitas linguagens de programação, incluindo TypeScript. Ela pode ser usada para fazer correções em um grande volume de código, alterar uma determinada palavra ou expressão em vários arquivos ou até mesmo para automatizar tarefas repetitivas. Ao aprender como realizar essa tarefa em TypeScript, você pode economizar muito tempo e esforço no seu processo de desenvolvimento.

## Como fazer a substituição de texto em TypeScript?

Para fazer a substituição de texto em TypeScript, você pode utilizar o método `replace()` da classe `String`. Ele aceita dois parâmetros: o padrão que você deseja substituir e o texto de substituição. Veja um exemplo prático:

```TypeScript
let texto = "Olá, sou um texto de exemplo."
let novoTexto = texto.replace("Olá", "Hi");
console.log(novoTexto);

// Output: Hi, sou um texto de exemplo.
```

Neste exemplo, usamos o método `replace()` para substituir a palavra "Olá" por "Hi" no texto original.

Também é possível usar expressões regulares para fazer substituições mais complexas. Veja outro exemplo:

```TypeScript
let texto = "Esse código foi escrito em TypeScript.";
let novoTexto = texto.replace(/TypeScript/g, "Python");
console.log(novoTexto);

// Output: Esse código foi escrito em Python.
```

Neste caso, usamos uma expressão regular para substituir todas as ocorrências da palavra "TypeScript" por "Python" no texto original.

## Uma visita mais aprofundada à substituição de texto

Existem diversas opções e possibilidades ao utilizar o método `replace()` em TypeScript. É possível fazer substituições sensíveis a maiúsculas e minúsculas, substituir apenas a primeira ocorrência de um padrão, utilizar funções de callback e muito mais. Recomendamos que você confira a documentação oficial da classe `String` para entender todas as opções disponíveis.

## Veja também

- [Documentação oficial do método `replace()` em TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#string-replace)
- [Outras dicas e truques em TypeScript](https://devblogs.microsoft.com/typescript/tag/tips-tricks/)