---
title:                "Javascript: Buscando e substituindo texto"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Por que usar a busca e substituição de texto em programação?

Algumas situações podem exigir a busca e substituição de texto em programação, como por exemplo, corrigir um erro ortográfico ou atualizar um determinado termo em um grande código fonte. A busca e substituição de texto é uma ferramenta útil e eficiente para realizar essas tarefas de forma rápida e automatizada.

## Como fazer a busca e substituição de texto em Javascript

A linguagem de programação Javascript oferece diferentes métodos para realizar a busca e substituição de texto. Abaixo mostramos alguns exemplos utilizando a função `replace()`, que recebe dois parâmetros: o texto que será buscado e o texto que substituirá as ocorrências do primeiro.

```
// Busca e substitui todas as ocorrências de "gato" por "cachorro"
let texto = "Eu adoro meu gato, ele é muito fofo!";
let novoTexto = texto.replace("gato", "cachorro");
console.log(novoTexto); // Output: Eu adoro meu cachorro, ele é muito fofo!

// Busca e substitui apenas a primeira ocorrência de "Javascript" por "Node.js"
let linguagem = "Eu estou aprendendo Javascript e é muito legal!";
let novaLinguagem = linguagem.replace("Javascript", "Node.js");
console.log(novaLinguagem); // Output: Eu estou aprendendo Node.js e é muito legal!
```

É possível também utilizar expressões regulares para uma busca mais específica e abrangente:

```
// Busca e substitui todas as letras maiúsculas por minúsculas
let texto = "Olá, Meu Nome é João!";
let novoTexto = texto.replace(/[A-Z]/g, (letra) => letra.toLowerCase());
console.log(novoTexto); // Output: olá, meu nome é joão!
```

## Profundidade na busca e substituição de texto

Além da função `replace()`, a linguagem Javascript possui outros métodos para realizar a busca e substituição de texto, como o `match()` e o `split()`, que utilizam expressões regulares como parâmetros. Esses métodos permitem uma manipulação mais precisa e complexa em relação ao texto original.

Também é importante entender o funcionamento das expressões regulares para um melhor desempenho na busca e substituição de texto. Elas permitem buscar padrões específicos dentro de uma string, indicando quais caracteres devem ser encontrados e em qual ordem.

# Veja também

- [Documentação oficial do Javascript sobre a função `replace()`](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Guia prático de expressões regulares em Javascript](https://www.devmedia.com.br/expressoes-regulares-no-javascript/37207)