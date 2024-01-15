---
title:                "Escrevendo para o erro padrão"
html_title:           "TypeScript: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Escrever para o erro padrão, ou standard error, pode ser útil em várias situações. Essa é uma forma de exibir mensagens de erro ou informações de depuração no console durante a execução do seu código TypeScript.

## Como Fazer

Para escrever para o erro padrão, você precisará utilizar a função `console.error()` do TypeScript. Dentro dessa função, você pode passar qualquer valor ou variável que deseja exibir no console como argumento. Veja um exemplo abaixo:

```TypeScript
const numero = 5;

console.error("Ops! O número inserido é: ", numero);
```

A saída desse código seria:

```
Ops! O número inserido é: 5
```

Você também pode usar o método `console.log()` para imprimir informações no console, mas a diferença é que ele escreve para a saída padrão, enquanto o `console.error()` escreve para o erro padrão.

## Mergulho Profundo

Quando se trata de depurar o seu código TypeScript, escrever para o erro padrão pode ser uma ferramenta extremamente útil. Por exemplo, você pode usar essa técnica para exibir o valor de uma variável em um ponto específico do seu código, para verificar se está seguindo a lógica esperada.

Além disso, se algum erro inesperado ocorrer durante a execução do seu código, escrever para o erro padrão pode te ajudar a identificar exatamente qual trecho do código está causando o problema.

No entanto, é importante lembrar que você não deve abusar dessa técnica e sempre deve remover ou comentar as linhas de código que escrevem para o erro padrão antes de dar commit no seu código final, para evitar que essas mensagens de depuração sejam exibidas ao usuário final da sua aplicação.

## Veja Também

- [Documentação oficial do TypeScript sobre a função console.error](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#more-erros-are-writing-to-standard-error)
- [Artigo da Dev Community sobre a diferença entre console.log() e console.error()](https://dev.to/anujsharmax/console-log-vs-console-error-vs-console-warn-the-ultimate-way-to-log-in-javascript-1k66)
- [Tutorial do DigitalOcean sobre depuração de código TypeScript](https://www.digitalocean.com/community/tutorials/how-to-debug-typescript-with-vscode)