---
title:                "Pesquisando e substituindo texto"
aliases:
- /pt/javascript/searching-and-replacing-text/
date:                  2024-01-20T17:58:06.613952-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

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
