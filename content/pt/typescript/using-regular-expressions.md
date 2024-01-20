---
title:                "Usando expressões regulares"
html_title:           "TypeScript: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O quê e Porquê?
Expressões regulares são padrões utilizados para buscar e substituir texto em uma string. Os programadores as usam para encontrar uma determinada sequência de caracteres em um texto, verificar sua validade ou modificá-lo de acordo com suas necessidades.

## Como fazer:
```TypeScript
// Busca pela palavra "exemplo" em uma string
const texto = "Este é um exemplo de utilização de expressões regulares."
const padrao = /exemplo/
console.log(padrao.test(texto)) // Saída: true

// Substituição de uma string por outra utilizando expressões regulares
const frase = "Eu vou viajar para o Japão no próximo mês."
const novaFrase = frase.replace(/Japão/, "Itália");
console.log(novaFrase) // Saída: Eu vou viajar para a Itália no próximo mês.
```

## Mergulho Profundo:
As expressões regulares surgiram na década de 1950 e são comumente associadas ao linguista americano Stephen Kleene e ao matemático alemão Heinz Hopcroft. Além de serem amplamente utilizadas na programação, as expressões regulares também são importantes em diversas outras áreas, como editoração de texto, extração de informações de documentos e processamento de linguagem natural. Alternativas para a utilização de expressões regulares incluem funções de manipulação de string, que podem ser mais simples de serem implementadas, porém menos eficientes quando se trata de tarefas complexas. No TypeScript, as expressões regulares são implementadas utilizando a classe RegExp.

## Veja Também:
- [Guia de Expressões Regulares em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)