---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Concatenação de strings é o processo de juntar duas ou mais strings para criar uma única string. Programadores fazem isso para manipular e formatar informações textuais de maneiras úteis.

## Como Fazer:

No TypeScript, você pode concatenar strings usando o operador `+` ou o método `concat()`, ou usando templates literals com `${}`. Aqui estão alguns exemplos:

```TypeScript
let primeiroNome = "João";
let sobrenome = "Silva";
// Usando o operador +
let nomeCompleto = primeiroNome + " " + sobrenome;
console.log(nomeCompleto); // Saída: João Silva

// Usando o método concat()
nomeCompleto = primeiroNome.concat(" ", sobrenome);
console.log(nomeCompleto); // Saída: João Silva

// Usando template literals
nomeCompleto = `${primeiroNome} ${sobrenome}`;
console.log(nomeCompleto); // Saída: João Silva
```

## Mergulhando Fundo

Historicamente, a concatenação de strings tem sido uma parte fundamental das linguagens de programação, remontando aos primeiros dias de COBOL e Fortran. No TypeScript e em muitas outras linguagens modernas, a concatenação de strings é otimizada para ser eficiente em termos de desempenho.

Existem outras maneiras de se juntar strings. Por exemplo, no TypeScript, você também pode usar o método `join()` de um array para concatenar strings:

```TypeScript
let partes = ["Hello", "World"];
let mensagem = partes.join(" ");
console.log(mensagem); // Saída: Hello World
```

A implementação exata da concatenação de strings pode variar dependendo do runtime JavaScript que você está usando. Por exemplo, no V8 (o motor JavaScript usado no Chrome e no Node.js), strings concatenadas são frequentemente representadas internamente como uma árvore de strings - isso pode fazer a concatenação de strings muito rápida!

## Veja Também:

- Documentação oficial do TypeScript sobre [Templates Strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [Stack Overflow: "How does JavaScript handle string concatenation internally?"](https://stackoverflow.com/questions/4886632/what-does-javascript-do-with-string-concatenation-does-it-join-on-strings-and-c)