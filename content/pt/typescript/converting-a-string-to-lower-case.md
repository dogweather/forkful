---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertendo uma string para letras minúsculas em TypeScript

## O que & Por quê?

Converter uma string para letras minúsculas significa transformar todas as letras maiúsculas de uma cadeia de texto para minúsculas. Programadores fazem isso para facilitar a comparação e classificação de strings, ignora-se a diferença entre maiúsculas e minúsculas.

## Como fazer:

Em TypeScript, você pode usar o método 'toLowerCase()' para converter uma string para letras minúsculas.

```TypeScript
let texto = "Olá Mundo!";
let textoMinusc = texto.toLowerCase();
console.log(textoMinusc);
```

Na execução desse código, você verá a saída:

```TypeScript
"olá mundo!"
```

## Mergulho Profundo:

A conversão de strings para letras minúsculas é uma prática antiga em programação, derivada desde o tempo das primeiras linguagens de programação. Em engenhos de pesquisa e bancos de dados, isso é fundamental para garantir a correspondência e ordenação correta dos dados.

Algumas alternativas para converter strings para minúsculas incluem loops que iteram sobre cada caracter, mas essas soluções são geralmente menos eficientes do que usar 'toLowerCase()'.

Em TypeScript, 'toLowerCase()' funciona perfeitamente com caracteres especiais e acentos em strings porque a linguagem suporta Unicode.

## Veja Também:

- Programação TypeScript: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- Documentação do método toLowerCase(): [https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)