---
title:                "Javascript: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Javascript?

Se você é um desenvolvedor Javascript, provavelmente já ouviu falar sobre expressões regulares, também conhecidas como "regex". Elas são uma ferramenta poderosa para lidar com strings e padrões em seu código. Neste post, vamos explorar por que as expressões regulares são úteis e como usá-las em seus projetos.

## Como usar expressões regulares em Javascript

As expressões regulares são representadas por um objeto RegExp em Javascript. Você pode criar este objeto usando o construtor `new RegExp()` ou simplesmente colocando seu padrão entre barras (`/pattern/`). Aqui está um exemplo de como podemos encontrar o número de ocorrências de uma determinada palavra em uma string usando uma expressão regular:

```Javascript
// Criando uma expressão regular para buscar a palavra "Javascript"
const regex = /Javascript/g;

// String de exemplo
const string = "Preciso aprender Javascript para aprimorar minhas habilidades de programação."

// Usando o método .match() para encontrar todas as ocorrências da palavra
const matches = string.match(regex);

// Output: ["Javascript"]
console.log(matches);
```

Existem vários métodos úteis que podemos usar com expressões regulares, como o `.test()` para verificar se um padrão existe em uma string e o `.replace()` para substituir partes de uma string. Além disso, podemos usar caracteres especiais, como `^` para encontrar um padrão no início de uma string e `$` para encontrar um padrão no final.

## Mergulhando nas expressões regulares

As expressões regulares podem ser um pouco complexas no início, mas é uma habilidade valiosa para qualquer desenvolvedor. Alguns dos conceitos importantes a serem compreendidos incluem:

- Metacaracteres: são caracteres especiais que têm significados específicos em expressões regulares, como `.` para representar qualquer caractere e `+` para representar a ocorrência de um ou mais caracteres.
- Grupos de captura: são usados para extrair partes específicas de uma string que correspondem a um padrão. Podemos definir grupos de captura usando parênteses `()`.
- Modificadores: são utilizados para alterar o comportamento de uma expressão regular, como `g` para fazer uma correspondência global em toda a string e `i` para ignorar diferenças entre letras maiúsculas e minúsculas.

Para se aprofundar mais nesse assunto, recomendo ler a documentação oficial do Javascript sobre expressões regulares e praticar com diferentes exemplos.

## Veja também

- ["Como dominar expressões regulares em Javascript" por Flavio Copes](https://flaviocopes.com/javascript-regular-expressions/)
- ["Expressões regulares em 30 minutos" por TylerMcGinnis](https://twitter.com/tylermcginnis/status/826840380940697344)

Espero que este post tenha ajudado você a entender melhor as expressões regulares e a usá-las em seu código Javascript. Com um pouco de prática, você estará dominando essa ferramenta poderosa em pouco tempo!