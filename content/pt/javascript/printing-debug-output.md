---
title:                "Imprimindo saída de depuração"
html_title:           "Javascript: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já encontrou um bug difícil de entender em seu código Javascript? Ou talvez você esteja tentando entender o fluxo de seu programa e não sabe o que está acontecendo em uma determinada linha de código? Nesses casos, imprimir saídas de depuração é uma técnica muito útil para entender melhor o que está acontecendo em seu código. Além disso, pode ajudar a identificar onde exatamente o problema está ocorrendo, facilitando a depuração e o processo de correção.

## Como Fazer

A impressão de saídas de depuração em Javascript é bastante simples e pode ser feita usando o comando `console.log ()`. Vamos dar uma olhada em alguns exemplos:

```
// Imprimindo uma mensagem simples
console.log("Olá mundo!");

// Imprimindo o valor de uma variável
let nome = "Ana";
console.log(`Olá ${nome}!`);

// Imprimindo uma mensagem de erro
console.error("Houve um erro no código.");
```

Agora, vamos ver como a saída desses comandos se parece no console do navegador:

```
Olá mundo!
Olá Ana!
Houve um erro no código.
```

Além disso, com o uso de parâmetros, é possível imprimir múltiplos valores e formatar a saída de acordo com suas necessidades. Por exemplo:

```
// Imprimindo múltiplos valores separados por vírgula
console.log("Meu nome é", nome, "e eu tenho", 25, "anos.");

// Imprimindo formatações diferentes
console.log("Este é um número:", 123, "e este é um texto:", "exemplo", ".");
```

Depois de executar esses comandos, a saída será a seguinte:

```
Meu nome é Ana e eu tenho 25 anos.
Este é um número: 123 e este é um texto: exemplo.
```

## Mergulho Profundo

Existem outros comandos disponíveis para imprimir saídas de depuração, como `console.warn ()` para mensagens de aviso e `console.info ()` para informações adicionais. Além disso, você pode usar o método `console.table ()` para exibir valores em forma de tabela, facilitando a visualização e a análise de informações mais complexas.

Além disso, é possível também configurar a quantidade de detalhes do registro exibidos pelo console. Por padrão, a quantidade de saída é configurada como "info", mas você pode alterá-la para exibir apenas erros com o comando `console.log(level, message)`. Por exemplo, se você só quiser ver mensagens de erro, pode usar:

```
console.log("error", "Houve um erro em algum lugar.");
```

Por fim, é importante lembrar que é uma boa prática remover ou desabilitar os comandos de impressão de saída de depuração ao terminar o processo de depuração e antes de publicar seu código. Isso evitará possíveis problemas e manterá seu código limpo e organizado.

## Veja Também

Aqui estão alguns links úteis para saber mais sobre impressão de saída de depuração em Javascript:

- [MDN Web Docs: Console](https://developer.mozilla.org/pt-BR/docs/Web/API/Console)
- [W3Schools: JavaScript Console](https://www.w3schools.com/js/js_console.asp)
- [DevDocs: Console API](https://devdocs.io/console/)