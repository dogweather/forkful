---
date: 2024-01-20 17:58:49.278091-07:00
description: "Como Fazer: Neste exemplo, usamos `replace` com uma `RegExp` para substituir\
  \ todas as ocorr\xEAncias da palavra \"mundo\" por \"planeta\"."
lastmod: '2024-04-05T21:53:46.641259-06:00'
model: gpt-4-1106-preview
summary: "Neste exemplo, usamos `replace` com uma `RegExp` para substituir todas as\
  \ ocorr\xEAncias da palavra \"mundo\" por \"planeta\"."
title: Pesquisando e substituindo texto
weight: 10
---

## Como Fazer:
```TypeScript
function substituirTexto(texto: string, busca: string, substituto: string): string {
  return texto.replace(new RegExp(busca, 'g'), substituto);
}

// Exemplo de uso:
const textoOriginal = "Olá, mundo! O mundo é vasto e mundo é a nossa casa.";
const textoModificado = substituirTexto(textoOriginal, "mundo", "planeta");

console.log(textoModificado);
// Saída: Olá, planeta! O planeta é vasto e planeta é a nossa casa.
```

Neste exemplo, usamos `replace` com uma `RegExp` para substituir todas as ocorrências da palavra "mundo" por "planeta".

## Mergulho Profundo
Historicamente, a necessidade de buscar e substituir em textos vem desde os primeiros editores de texto e ambientes de programação. Alternativas para a função `replace` incluem o uso de bibliotecas de manipulação de strings, ferramentas de linha de comando como `sed`, ou até recursos nativos de editores de texto e IDEs.

A implementação do `replace` no JavaScript e, por extensão, no TypeScript faz parte dos objetos `String`. Ela pode ser simples ou complexa, usando strings literais ou expressões regulares (RegExp) para encontrar padrões mais sofisticados. Importante: a função `replace()` por padrão substitui apenas a primeira ocorrência, a não ser que uma flag global 'g' seja usada na `RegExp`.

## Veja Também:
- MDN Web Docs sobre a função `replace`: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Guia de expressões regulares em JavaScript para iniciantes: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Documentação TypeScript: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- Uma interessante ferramenta online para testar suas RegExp: [https://regex101.com/](https://regex101.com/)
