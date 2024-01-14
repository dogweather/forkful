---
title:                "TypeScript: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que utilizar a escrita em stderr

Escrever em stderr é uma prática comum na programação TypeScript. Isso porque, em determinadas situações, pode ser mais útil e eficiente enviar mensagens de erro para esse canal, ao invés da saída padrão. Isso permite que os desenvolvedores capturem e gerenciem esses erros de forma mais específica.

## Como fazer isso

Para escrever em stderr em TypeScript, é necessário utilizar o objeto `process`. Primeiro, é preciso importar esse objeto através do módulo `node`. Em seguida, basta chamar a função `stderr.write()` e fornecer a mensagem que deseja enviar.

```TypeScript
import * as process from 'node';

process.stderr.write("Mensagem de erro");
```

Isso irá enviar a mensagem "Mensagem de erro" para o canal de erro. Além disso, é possível fornecer um callback para essa função, para tratar o erro ou realizar outras operações após o envio da mensagem.

## Mergulho profundo

Ao escrever em stderr, é importante entender que esse canal é utilizado para mensagens de erro e é considerado uma saída não esperada. Portanto, é essencial garantir que essas mensagens sejam gerenciadas e tratadas corretamente durante o processo de desenvolvimento e de produção da aplicação.

Além disso, é importante ter em mente que mensagens de erro em stderr só serão exibidas caso haja algum erro no código. Por isso, é necessário ter um bom entendimento de como gerenciar e tratar erros no TypeScript.

## Veja também
- [Documentação oficial do TypeScript sobre o objeto `process`](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-8.html)
- [Artigo sobre Error Handling em TypeScript](https://blog.logrocket.com/error-handling-in-typescript/)
- [Curso gratuito de TypeScript](https://www.freecodecamp.org/news/learn-typescript-in-5-minutes-13eda868daeb/)