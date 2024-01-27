---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string é transformar todas as letras iniciais de palavras numa cadeia de texto para maiúsculas. Programadores fazem isso para formatar títulos, cabeçalhos ou para atender a normas específicas de apresentação de dados.

## How to:
```Javascript
function capitalizarString(texto) {
  return texto.replace(/(^|\s)\S/g, letra => letra.toUpperCase());
}

console.log(capitalizarString('olá, mundo!')); // Olá, Mundo!

// Usando funções de string do ECMAScript 2021
console.log('olá, mundo!'.replace(/\b\w/g, char => char.toUpperCase())); // Olá, Mundo!
```

## Deep Dive
Historicamente, capitalizar palavras em textos é uma prática que remonta à criação do livro impresso para diferenciar seções ou iniciar frases. No mundo da programação, a função para capitalização de strings em JavaScript não é nativa e precisa ser criada manualmente ou obtida de bibliotecas.

Duas abordagens são comuns: a primeira usa uma expressão regular combinada com a função `replace()` para encontrar o primeiro caractere de cada palavra e convertê-lo em maiúsculo. A segunda, mais moderna e introduzida no ECMAScript 2021, utiliza métodos de cadeia de caracteres (string) para um resultado semelhante com uma sintaxe mais limpa.

Apesar da simplicidade, a capitalização em JavaScript deve levar em conta peculiaridades, como a possibilidade de caracteres especiais, números ou símbolos aparecerem no início de uma palavra, o que pode exigir ajustes na expressão regular.

Alternativas incluem o uso de bibliotecas como Lodash com sua função `_.startCase` para obter um resultado similar com uma linha de código:

```Javascript
_.startCase('olá, mundo!'); // Olá Mundo!
```

No entanto, o uso de bibliotecas para tal tarefa simples pode ser um exagero, então saber como implementar a capitalização manualmente é uma habilidade útil.

## See Also
- MDN Web Docs sobre `String.prototype.replace()`: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Documentação do Lodash `_.startCase`: https://lodash.com/docs/#startCase
- ECMAScript 2021 specifications: https://www.ecma-international.org/publications-and-standards/standards/ecma-262/
