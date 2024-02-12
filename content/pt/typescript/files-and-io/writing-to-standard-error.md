---
title:                "Escrevendo para o erro padrão"
aliases:
- /pt/typescript/writing-to-standard-error.md
date:                  2024-02-03T19:34:40.545678-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Em TypeScript, escrever para o erro padrão (stderr) é um processo de enviar mensagens de erro ou logs diretamente para o fluxo de saída de erro do ambiente (por exemplo, o console em node.js ou um navegador web). Isso é essencial para diagnosticar problemas sem interferir com a saída padrão (stdout), que é tipicamente usada para dados de programas, garantindo que o tratamento de erros e o registro de logs sejam gerenciados de forma eficiente e coesa.

## Como Fazer:
TypeScript, sendo um superconjunto de JavaScript, depende do ambiente de execução JS subjacente (como Node.js) para escrever no stderr. Aqui está como você pode fazer isso diretamente:

```typescript
console.error("Esta é uma mensagem de erro.");
```

Exemplo de saída para stderr:
```
Esta é uma mensagem de erro.
```

Em um ambiente Node.js, você também pode usar o método `process.stderr.write()` para uma escrita mais de baixo nível:

```typescript
process.stderr.write("Mensagem de erro de baixo nível.\n");
```

Exemplo de saída para stderr:
```
Mensagem de erro de baixo nível.
```

Para um registro de erros mais estruturado, você pode usar bibliotecas de terceiros populares, como `winston` ou `pino`. Aqui está como registrar erros usando o `winston`:

Primeiro, instale o `winston`:

```bash
npm install winston
```

Em seguida, use-o no seu arquivo TypeScript:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Erro registrado usando o winston.');
```

Isso escreverá o erro tanto no console quanto em um arquivo chamado `error.log`. Lembre-se, ao escrever em arquivos, é importante gerenciar as permissões de arquivo e a rotação para evitar problemas relacionados ao uso do espaço em disco.
