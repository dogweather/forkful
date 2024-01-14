---
title:    "TypeScript: Escrevendo para o erro padrão"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Por que escrever no erro padrão?

Escrever para o erro padrão pode ser extremamente útil durante o processo de desenvolvimento de um projeto em TypeScript. Ele permite que você envie mensagens de erro e depuração para a saída padrão, o que ajuda a identificar possíveis problemas e aperfeiçoar seu código.

## Como fazer?

Para escrever no erro padrão em TypeScript, basta utilizar a função "console.error()" seguida da mensagem que você deseja exibir. Por exemplo:

```TypeScript
console.error("Erro: Variável x não definida");
```

A saída no console seria:

```
Erro: Variável x não definida
```

Você também pode utilizar variáveis e expressões dentro da função "console.error()". Por exemplo:

```TypeScript
let mensagem = "Bem-vindo ao meu blog!";
console.error("Mensagem:", mensagem);
```

A saída no console seria:

```
Mensagem: Bem-vindo ao meu blog!
```

## Aprofundando

Além de exibir mensagens de erro, você também pode utilizar a função "console.error()" para depuração de código. É possível incluir informações sobre variáveis e estados do programa para diagnosticar possíveis problemas. Por exemplo:

```TypeScript
let x = 10;
let y = 20;
console.error("Valor de x:", x);
console.error("Valor de y:", y);
```

A saída no console seria:

```
Valor de x: 10
Valor de y: 20
```

Isso pode ser extremamente útil ao tentar descobrir por que um determinado trecho de código não está funcionando corretamente.

# Veja também

Para mais informações e dicas sobre o uso do TypeScript, confira os seguintes links:

- https://www.typescriptlang.org/
- https://www.typescriptlang.org/docs/handbook/basic-types.html
- https://www.freecodecamp.org/news/the-typescript-handbook/
- https://www.tutorialspoint.com/typescript/index.htm

Esperamos que este artigo tenha sido útil e que você possa utilizar a função "console.error()" em seus projetos em TypeScript para tornar o processo de desenvolvimento mais fácil e eficiente. Obrigado por ler e até a próxima!