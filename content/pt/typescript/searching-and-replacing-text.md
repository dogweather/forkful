---
title:                "TypeScript: Localizando e substituindo texto"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Substituir texto é uma tarefa essencial na programação, seja para corrigir erros ou para fazer alterações em massa. Aprender como realizar essa tarefa usando TypeScript pode ajudar a tornar seu código mais eficiente e preciso.

## Como Fazer

Para substituir texto em TypeScript, podemos usar o método `replace()` do objeto `String`. Ele aceita dois parâmetros: o texto que queremos substituir e o novo texto que queremos inserir. Vamos ver um exemplo:

```TypeScript
let mensagem: string = "Bem-vindo ao blog de programação!"
let novaMensagem: string = mensagem.replace("blog de programação", "blog de TypeScript")
console.log(novaMensagem)
```

A saída seria: "Bem-vindo ao blog de TypeScript!".

Podemos também usar expressões regulares no método `replace()` para realizar substituições mais complexas. Por exemplo, se quisermos substituir todas as ocorrências de uma palavra por outra, podemos usar a flag `g` para indicar que a substituição deve ser global. Veja o exemplo:

```TypeScript
let texto: string = "Este artigo é sobre TypeScript e como substituir texto."
let novoTexto: string = texto.replace(/TypeScript/g, "programação")
console.log(novoTexto)
```

A saída seria: "Este artigo é sobre programação e como substituir texto.".

## Deep Dive

Existem alguns outros detalhes importantes a serem considerados quando se trata de substituir texto em TypeScript. Por exemplo, o método `replace()` retorna uma nova string, não altera a string original. Além disso, ele é sensível a maiúsculas e minúsculas, o que significa que se usarmos a palavra "programação" em vez de "TypeScript" no exemplo acima, a substituição não será feita.

Outra coisa a ter em mente é que expressões regulares podem ser poderosas, mas também podem ser complicadas. Certifique-se de pesquisar e entender como elas funcionam antes de usá-las em seus códigos.

## Veja também

- [Documentação oficial do método `replace()` em TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#template-literal-types)
- [Guia de expressões regulares em JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)