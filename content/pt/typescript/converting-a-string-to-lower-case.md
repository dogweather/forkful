---
title:    "TypeScript: Convertendo uma string para letra minúscula"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

Há muitos cenários em que é útil converter uma string para minúsculas. Isso pode ser necessário para processar dados, realizar comparações ou até mesmo para exibir conteúdo em uma interface de usuário.

## Como fazer

Aqui estão alguns exemplos de como converter uma string para minúsculas em TypeScript:

```typescript
// Exemplo 1
const string = "CONVERTER PARA MINÚSCULAS";
console.log(string.toLowerCase()); // saída: converter para minúsculas

// Exemplo 2
const string2 = "Texto aleatório 123";
console.log(string2.toLowerCase()); // saída: texto aleatório 123
```

O método `toLowerCase()` é utilizado para converter uma string para minúsculas. Ele retorna uma nova string com os caracteres convertidos. É importante lembrar que esse método não altera a string original, apenas retorna uma nova versão convertida.

## Mergulho Profundo

Ao converter uma string para minúsculas, é preciso levar em consideração o conjunto de caracteres (charset) utilizado. Por exemplo, em inglês, a string "I" se torna "i" na forma minúscula, mas em outras línguas, pode haver variações nas letras minúsculas.

Além disso, o método `toLowerCase()` não afeta números e caracteres especiais, convertendo apenas as letras para minúsculas.

## Veja também

- [Documentação oficial do método `toLowerCase()` em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Artigo sobre manipulação de strings em TypeScript](https://www.tutorialsteacher.com/typescript/string)
- [Exemplos práticos de conversão de strings em minúsculas com TypeScript](https://www.tutorialspoint.com/typescript/typescript_string_tolowercase.htm)