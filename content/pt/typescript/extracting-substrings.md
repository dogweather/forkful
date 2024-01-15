---
title:                "Extraindo Substrings"
html_title:           "TypeScript: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que 

Alguns cenários podem exigir que extraiamos substrings de uma string maior. Isso pode ser útil ao trabalhar com dados de entrada, como números de telefone ou endereços de email, ou ao manipular dados de texto em geral.

## Como Fazer 

```TypeScript
let phrase: string = "Olá mundo!";
let substring1: string = phrase.substring(4); 

console.log(substring1); // Saída: mundo!
```

A função `substring` em TypeScript nos permite extrair uma parte de uma string a partir de um índice específico. O primeiro argumento é o índice de início da substring e o segundo (opcional) é o índice final.

Podemos também usar a função `slice` de maneira semelhante:

```TypeScript
let phrase: string = "Hello World!";
let substring2: string = phrase.slice(6, 11); 

console.log(substring2); // Saída: World
```

A diferença é que `substring` não pode aceitar índices negativos, enquanto `slice` permite usar índices negativos para contar a partir do final da string.

## Deep Dive 

Existem algumas coisas a serem consideradas ao extrair substrings em TypeScript. O primeiro é que ambas as funções vão retornar uma nova string e não modificam a string original.

Além disso, se passarmos apenas um argumento para `substring`, ele vai extrair a substring do índice fornecido até o final da string. Mas se passarmos apenas um argumento para `slice`, ele vai extrair do índice fornecido até um índice antes do final da string.

Outra diferença é quando os índices fornecidos estão fora do intervalo da string. Nesse caso, `substring` vai ajustar os índices para dentro do intervalo válido da string, enquanto `slice` vai retornar uma string vazia.

## Veja Também 

- Documentação oficial do TypeScript sobre `substring` e `slice`: https://www.typescriptlang.org/docs/handbook/intro.html
- Tutorial sobre manipulação de strings: https://www.w3schools.com/js/js_strings.asp