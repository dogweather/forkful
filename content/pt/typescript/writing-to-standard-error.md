---
title:                "TypeScript: Escrevendo no erro padrão"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão pode ser uma técnica útil para depurar e identificar problemas em seu código TypeScript. Ao enviar informações de erro para o erro padrão, você pode obter uma visão mais clara de onde o problema está ocorrendo, o que pode facilitar a resolução de bugs.

## Como fazer:

Para escrever para o erro padrão em TypeScript, você pode usar o método `console.error()`. Este método aceita como argumento a mensagem de erro que você deseja enviar para o erro padrão. Veja um exemplo abaixo:

```TypeScript
console.error("Um erro ocorreu! Verifique o código para identificar o problema.");
```

A saída do código acima seria algo como:

`Um erro ocorreu! Verifique o código para identificar o problema.`

Você também pode incluir variáveis ou outros valores em sua mensagem de erro, usando a sintaxe de string de modelo. Veja um exemplo abaixo:

```TypeScript
const numero = 10;
console.error(`Erro! O número ${numero} não pode ser dividido por zero.`);
```

A saída seria:

`Erro! O número 10 não pode ser dividido por zero.`

## Profundando mais:

Além do método `console.error()`, o TypeScript também possui outras formas de lidar com erros e exceções. Você pode usar a declaração `try...catch` para capturar e tratar erros em seu código. Veja um exemplo abaixo:

```TypeScript
try {
  // código que pode gerar um erro
} catch(error) {
  // tratamento do erro
  console.error(error); // envia o erro para o erro padrão
}
```

Você também pode criar suas próprias classes de erro personalizadas para capturar e gerenciar erros específicos em seu código. Isso pode ser especialmente útil em projetos maiores e complexos.

## Veja também:

- [Documentação oficial do TypeScript sobre tratamento de erros](https://www.typescriptlang.org/docs/handbook/2/typescript-in-javascript#handling-errors)
- [Artigo sobre tratamento de exceções em TypeScript](https://medium.com/@abhinavroshan/exception-handling-in-typescript-bf71262cfb36)
- [Vídeo tutorial sobre como lidar com erros em TypeScript](https://www.youtube.com/watch?v=jnEAzUw0pJg)