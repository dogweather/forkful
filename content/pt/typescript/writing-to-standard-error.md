---
title:    "TypeScript: Escrevendo para o erro padrão"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma prática comum em programação TypeScript. Isso permite que os desenvolvedores capturem e exibam informações de erro precisas, facilitando a depuração e o aprimoramento do código. Além disso, escrever para o erro padrão também pode ser útil para fins de registro e monitoramento de erros em aplicativos.

## Como fazer

Para escrever para o erro padrão em TypeScript, você pode usar o objeto `console` e o método `error()`. Aqui está um exemplo simples de código:

```TypeScript
console.error("Este é um erro de exemplo");
```

O código acima irá imprimir a mensagem de erro no console e será exibido com um formato visualmente diferente do que as mensagens normais do console. Aqui está o output desta linha de código:

<img src="https://i.imgur.com/CAIrP5c.png" alt="Exemplo de erro de console" width="400"/>

Você também pode incluir informações adicionais na mensagem de erro, como uma variável ou uma descrição mais detalhada do erro:

```TypeScript
const numero = 10;
console.error(`O valor da variável "numero" é ${numero}. Este é um erro de exemplo.`);
```

O output deste código será:

<img src="https://i.imgur.com/c7NjhOr.png" alt="Exemplo de erro de console com variável" width="400"/>

Você também pode usar a sintaxe try/catch para capturar erros específicos e escrevê-los para o erro padrão. Aqui está um exemplo:

```TypeScript
try {
  const lista = ["um", "dois", "três"];
  console.log(lista[3]); // O índice 3 não existe neste array
} catch (err) {
  console.error(`Erro: ${err}`); // Imprime o erro específico
}
```

O output será:

<img src="https://i.imgur.com/E3pcX5V.png" alt="Exemplo de captura de erro" width="400"/>

## Mergulho Profundo

É importante lembrar que escrever para o erro padrão não é o único meio de lidar com erros em seu código TypeScript. Você também pode usar a declaração `throw` para lançar um erro em certas condições e tratá-lo com `try/catch`. Além disso, você pode personalizar a formatação da mensagem de erro usando o objeto `Error` e seu construtor. Para mais informações sobre tratamento de erros em TypeScript, recomendamos a leitura da documentação oficial.

## Veja Também

- [Documentação oficial do TypeScript sobre tratamento de erros](https://www.typescriptlang.org/docs/handbook/errors.html)
- [Guia sobre tratamento de erros em TypeScript](https://levelup.gitconnected.com/how-to-handle-errors-in-typescript-like-a-pro-40896411fc27)
- [Tutorial de formatação de erros em TypeScript](https://www.digitalocean.com/community/tutorials/typescript-custom-error-objects)