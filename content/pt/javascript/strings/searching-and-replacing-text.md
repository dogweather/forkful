---
date: 2024-01-20 17:58:06.613952-07:00
description: "Procurar e substituir texto \xE9 como uma opera\xE7\xE3o de busca e\
  \ troca, onde um trecho de texto \xE9 localizado e trocado por outro. Programadores\
  \ fazem isso para\u2026"
lastmod: '2024-03-13T22:44:46.948252-06:00'
model: gpt-4-1106-preview
summary: "Procurar e substituir texto \xE9 como uma opera\xE7\xE3o de busca e troca,\
  \ onde um trecho de texto \xE9 localizado e trocado por outro. Programadores fazem\
  \ isso para\u2026"
title: Pesquisando e substituindo texto
weight: 10
---

## O Que é e Por Que?

Procurar e substituir texto é como uma operação de busca e troca, onde um trecho de texto é localizado e trocado por outro. Programadores fazem isso para atualizar dados, corrigir erros, ou manipular informações de forma eficaz.

## Como Fazer:

Vamos ver como JavaScript lida com isso usando o método `replace` de strings.

```javascript
let texto = "As raposas são astutas e as corujas, sábias.";

// Substituir uma palavra
let novoTexto = texto.replace("raposas", "gatos");
console.log(novoTexto);  // "As gatos são astutas e as corujas, sábias."

// Substituir todas as ocorrências com regex global (g)
novoTexto = texto.replace(/as/g, "os");
console.log(novoTexto);  // "Os raposas são astutos e os corujas, sábios."

// Substituir utilizando uma função de callback
novoTexto = texto.replace(/(\b[a-zA-Z]+\b)/g, function(match){
  return match.toUpperCase();
});
console.log(novoTexto);  // "AS RAPOSAS SÃO ASTUTAS E AS CORUJAS, SÁBIAS."
```

## Imersão:

A busca e substituição de texto é uma técnica antiga no mundo da programação, essencial desde os primeiros editores de texto. No JavaScript, antes do ECMAScript 5, usávamos expressões regulares e métodos como `indexOf` e `substring` para manipular strings - era funcional, mas nada elegante.

Alternativas modernas como `replaceAll` vieram para simplificar o processo, oferecendo um jeito mais direto para substituir todas as ocorrências sem usar regex. Mas lembre-se, regex oferece poder e flexibilidade para padrões complexos que não podem ser ignorados.

Quanto à implementação, a função de `replace` trabalha internamente copiando a string original e realizando as substituições onde os padrões são encontrados, resultando numa nova string.

## Veja Também:

- MDN Web Docs para `replace()`: [developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Expressões Regulares (Regex) em JavaScript: [developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Regular_Expressions)
- Documentação sobre `replaceAll()`: [developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll](https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replaceAll)
