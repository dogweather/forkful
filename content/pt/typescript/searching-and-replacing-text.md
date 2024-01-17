---
title:                "Busca e substituição de texto"
html_title:           "TypeScript: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

O que e por que?
Buscar e substituir texto é um processo comum na programação, onde os desenvolvedores procuram por uma determinada string em um código e a substituem por outra. Isso é útil para fazer mudanças rápidas e consistentes em um código. Programadores geralmente fazem isso para economizar tempo e evitar erros manuais ao fazer alterações em um grande conjunto de arquivos.

Como fazer:
```TypeScript
// Exemplo de busca e substituição em uma string
let texto = "Olá mundo!";
let novoTexto = texto.replace("mundo", "amigos");

console.log(novoTexto); // Saída: "Olá amigos!"
```
```TypeScript
// Exemplo de busca e substituição em um array
let numeros = [1, 2, 3, 4, 5];
numeros.forEach((numero, indice) => {
  if(numero % 2 === 0){
    numeros[indice] = "par";
  } else {
    numeros[indice] = "ímpar";
  }
});

console.log(numeros); // Saída: ["ímpar", "par", "ímpar", "par", "ímpar"]
```

Mergulho profundo:
A busca e substituição de texto tem sido uma técnica utilizada por programadores desde os primeiros dias da programação. Antes da existência de ferramentas avançadas de edição de texto, os desenvolvedores tinham que fazer tais mudanças manualmente em cada ocorrência de uma string. No entanto, hoje há muitas ferramentas e opções disponíveis para simplificar esse processo, incluindo utilitários de linha de comando e IDEs com recursos integrados de busca e substituição.

Veja também:
- [Documentação oficial do TypeScript sobre busca e substituição](https://www.typescriptlang.org/docs/handbook/utility-types.html#recordt)
- [Artigo sobre busca e substituição com VS Code](https://code.visualstudio.com/docs/editor/codebasics#_find-and-replace)