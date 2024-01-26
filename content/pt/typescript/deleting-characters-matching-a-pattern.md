---
title:                "Excluindo caracteres que correspondem a um padrão"
date:                  2024-01-20T17:43:05.985271-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deletar caracteres que correspondem a um padrão significa usar algoritmos para encontrar e remover certos caracteres de uma string. Fazemos isso para limpar dados, preparar texto para processamento ou remover informações desnecessárias ou sensíveis.

## How to:
Para excluir caracteres usando um padrão no TypeScript, você geralmente vai usar expressões regulares com o método `.replace()`. Aqui estão alguns exemplos:

```typescript
let texto: string = "Olá, Dev12345!";

// Remove todos os dígitos.
let semNumeros: string = texto.replace(/\d+/g, '');
console.log(semNumeros); // Saída: "Olá, Dev!"

// Remove todas as letras.
let semLetras: string = texto.replace(/[A-Za-z]+/g, '');
console.log(semLetras); // Saída: ", 12345!"

// Remove todo caractere que não seja letra ou número.
let apenasAlfanumericos: string = texto.replace(/[^A-Za-z0-9]+/g, '');
console.log(apenasAlfanumericos); // Saída: "OláDev12345"
```

## Deep Dive
A necessidade de deletar caracteres específicos das strings é tão antiga quanto as próprias linguagens de programação. As expressões regulares, originadas na década de 1950, são uma ferramenta poderosa para realizar essa tarefa. Alternativas modernas incluem o uso de funções de alto nível como `.filter()` para arrays, quando se está trabalhando com coleções de caracteres.

Em TypeScript, o método `.replace()` pode ser configurado para ser global (com a flag `'g'`) e é frequentemente combinado com expressões regulares para identificar os padrões a serem removidos. A compreensão precisa de expressões regulares é crítica para a manipulação eficaz de texto, e a prática leva à perfeição aqui.

## See Also
- [Mozilla Developer Network - Expressões Regulares](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Documentation - String Manipulation](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [Regex101 - Ferramenta online para teste de expressões regulares](https://regex101.com/)
