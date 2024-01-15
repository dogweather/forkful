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

## Por que usar expressões regulares em TypeScript?

Expressões regulares são uma ferramenta poderosa para busca e manipulação de padrões de texto em strings. Elas podem ser úteis em várias situações, como validar formulários, filtrar conteúdo ou extrair informações específicas de um texto. Além disso, as expressões regulares são suportadas nativamente em TypeScript, tornando sua utilização ainda mais fácil e eficiente.

## Como usar expressões regulares em TypeScript?

```TypeScript
const texto = "Este é um exemplo de texto com várias palavras";

// Encontrando todas as ocorrências de "texto"
const regex = /texto/g;
const resultados = texto.match(regex);

console.log(resultados);
// Output: ["texto", "texto"]

// Substituindo a primeira ocorrência de "texto" por "exemplo"
const novoTexto = texto.replace(regex, "exemplo");

console.log(novoTexto);
// Output: "Este é um exemplo de texto com várias palavras"

// Verificando se o texto contém alguma das palavras "exemplo" ou "teste"
const regex2 = /exemplo|teste/;
const resultado = regex2.test(texto);

console.log(resultado);
// Output: true
```

## Aprofundando em expressões regulares

As expressões regulares em TypeScript seguem a mesma sintaxe do JavaScript, o que pode ser um pouco confuso para quem não está familiarizado. Porém, uma vez que se compreende a estrutura básica, é possível criar padrões muito específicos e complexos. Além disso, também é possível utilizar o conceito de grupos e referências para substituir partes específicas do texto encontrado.

## Veja também

- [Documentação oficial do TypeScript sobre expressões regulares](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Ferramenta online para testar expressões regulares](https://regex101.com/)
- [Tutorial em vídeo sobre expressões regulares em TypeScript](https://www.youtube.com/watch?v=7Vtl2WggqOg)