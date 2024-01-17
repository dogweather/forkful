---
title:                "Colocando a primeira letra em maiúscula de uma string"
html_title:           "TypeScript: Colocando a primeira letra em maiúscula de uma string"
simple_title:         "Colocando a primeira letra em maiúscula de uma string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

O que e por que?

Capitalizar uma string em TypeScript significa transformar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para melhorar a legibilidade de seus códigos e torná-los mais padrão e organizado.

Como fazer:

```
// Exemplo 1
const string = "aprendendo tiposcript";
const capitalizedString = string.replace(/\b\w/g, (c) => c.toUpperCase());
console.log(capitalizedString);
// Output: Aprendendo TypeScript

// Exemplo 2
const string2 = "olá mundo";
const capitalizedString2 = string2.charAt(0).toUpperCase() + string2.slice(1);
console.log(capitalizedString2);
// Output: Olá mundo
```

Deep Dive:

1. Contexto histórico: A prática de capitalizar strings vem de linguagens de programação mais antigas, onde usar letras maiúsculas era uma forma de diferenciar variáveis de palavras reservadas.

2. Alternativas: Alguns programadores preferem usar todas as letras minúsculas em suas strings para manter uma estética mais uniforme. Outros podem optar por capitalizar apenas a primeira letra de uma string, em vez de todas as palavras.

3. Detalhes de implementação: Existem várias maneiras de capitalizar uma string em TypeScript, como mostrado nos exemplos acima. Algumas podem ser mais eficientes em termos de desempenho do que outras, dependendo do tamanho da string e do número de palavras.

Veja também:

- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial Completo do TypeScript para Iniciantes](https://medium.com/collabcode/typescript-o-guia-completo-para-iniciantes-2a3a2ebe0804)
- [Por que Capitalizar Strings é Importante em Programação](https://levelup.gitconnected.com/why-is-capitalizing-strings-important-in-programming-d02a003fd3f)