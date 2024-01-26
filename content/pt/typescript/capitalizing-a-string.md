---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string significa converter a primeira letra de cada palavra para maiúscula, mantendo as restantes em minúscula. Programadores fazem isso para formatar textos, títulos ou para garantir consistência nos dados, como nomes próprios.

## How to:
Em TypeScript, você pode usar o método `.replace()` junto com expressões regulares para capitalizar palavras, ou criar uma função dedicada. Aqui tá um exemplo de como fazer isso:

```TypeScript
function capitalizeString(input: string): string {
  return input.replace(/\b\w/g, letter => letter.toUpperCase());
}

let title = 'olá, mundo do typescript';
let capitalizedTitle = capitalizeString(title);

console.log(capitalizedTitle); // Saída: 'Olá, Mundo Do Typescript'
```

Esse código vai deixar sua string bem bonita.

## Deep Dive
Historicamente, capitalizar strings é algo que vem dos tempos da imprensa onde títulos de livros, jornais e trabalhos acadêmicos possuíam a primeira letra de cada palavra em maiúscula para chamar atenção e criar um padrão visual. Em programação, existem várias maneiras de capitalizar strings dependendo da linguagem.

No TypeScript, como é uma linguagem que compila para JavaScript, as mesmas técnicas geralmente aplicadas em JavaScript são válidas. Alternativas à função customizada incluem o uso de bibliotecas como Lodash com seu método `.capitalize()` que capitaliza apenas a primeira letra da string inteira.

A implementação do método `.replace()` com uma expressão regular é uma das abordagens mais diretas, e ela funciona bem porque JavaScript (e por extensão TypeScript) suporta as expressões regulares nativamente. A expressão `\b\w` pega o limite de uma palavra (`\b`) e o primeiro caractere alfanumérico (`\w`), permitindo a capitalização.

## See Also
- [MDN Web Docs on replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Lodash capitalize method](https://lodash.com/docs/4.17.15#capitalize)
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)

Assim você explora a fundo o poder das strings e faz os textos brilharem no universo do TypeScript!
