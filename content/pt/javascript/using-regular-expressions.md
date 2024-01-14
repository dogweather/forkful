---
title:                "Javascript: Utilizando expressões regulares"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares no Javascript?

Expressões Regulares (também conhecidas como RegEx) são ferramentas poderosas que permitem encontrar padrões em texto. No Javascript, elas são muito úteis para validar dados de formulários, filtrar ou substituir strings e até mesmo para criar expressões de busca complexas. Se você é um programador Javascript, aprender a usar Expressões Regulares pode facilitar muito a sua vida.

## Como utilizar Expressões Regulares no Javascript

Para utilizar Expressões Regulares no Javascript, é necessário criar um objeto RegExp ou utilizar o método nativo `match()`, `search()`, `replace()` ou `split()`. Vamos ver alguns exemplos de como utilizar cada um desses métodos:

```Javascript
// Criando um objeto RegExp
let regex = new RegExp("palavra");
let texto = "Essa string contém a palavra exemplo";

regex.test(texto);
// Output: true

// Utilizando o método match()
let texto2 = "Existem muitas palavras repetidas nesse texto";

texto2.match(/palavras/g);
// Output: ["palavras", "palavras"]

// Utilizando o método search()
let texto3 = "javascript é uma linguagem de programação";

texto3.search(/java/i);
// Output: 0 (índice da primeira ocorrência)

// Utilizando o método replace()
let texto4 = "Hoje é dia de aprender regex";

console.log(texto4.replace(/regex/g, "Expressões Regulares"));
// Output: Hoje é dia de aprender Expressões Regulares

// Utilizando o método split()
let texto5 = "Que tal separar essa string em palavras?";

texto5.split(/ /g);
// Output: ["Que", "tal", "separar", "essa", "string", "em", "palavras?"]
```

## Mergulho profundo: dicas e truques para Expressões Regulares

Aqui estão algumas dicas para aprofundar ainda mais o seu conhecimento em Expressões Regulares no Javascript:

- Utilize a flag `g` para realizar uma busca global (encontrar todas as ocorrências) ou `i` para ignorar maiúsculas e minúsculas.
- Utilize os operadores `|` (OU), `[]` (intervalos de caracteres) e `()` (grupos) para deixar suas expressões mais complexas.
- Utilize os quantificadores `*` (zero ou mais repetições), `+` (uma ou mais repetições) e `?` (zero ou uma repetição) para buscar padrões repetidos.
- Utilize os metacaracteres `\w` (qualquer caractere alfanumérico), `\d` (qualquer dígito), `\s` (qualquer espaço em branco) e `\b` (fronteira entre palavras) para criar expressões mais precisas.
- Utilize as funções de callback para realizar tarefas mais complexas, como substituir o conteúdo encontrado usando uma função personalizada.

Para saber mais sobre Expressões Regulares no Javascript, confira a documentação oficial da Mozilla: [Javascript Regular Expressions](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/RegExp).

## Veja também

- [Regex101](https://regex101.com/) - ferramenta online para testar e aprender sobre Expressões Regulares.
- [Mastering Regular Expressions](https://www.amazon.com.br/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124) - livro que aprofunda o conhecimento sobre RegEx em diversas linguagens de programação.