---
aliases:
- /pt/javascript/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:34.944144-07:00
description: "Escrever para o erro padr\xE3o (stderr) em JavaScript trata de direcionar\
  \ mensagens de erro ou qualquer informa\xE7\xE3o cr\xEDtica para um fluxo espec\xED\
  fico e\u2026"
lastmod: 2024-02-18 23:08:58.548363
model: gpt-4-0125-preview
summary: "Escrever para o erro padr\xE3o (stderr) em JavaScript trata de direcionar\
  \ mensagens de erro ou qualquer informa\xE7\xE3o cr\xEDtica para um fluxo espec\xED\
  fico e\u2026"
title: "Escrevendo para o erro padr\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever para o erro padrão (stderr) em JavaScript trata de direcionar mensagens de erro ou qualquer informação crítica para um fluxo específico e separado, o que é especialmente útil em ambientes semelhantes ao Unix para fins de registro e depuração. Programadores fazem isso para diferenciar a saída normal do programa das mensagens de erro, permitindo uma gestão mais limpa da saída e monitoramento de erros mais fácil.

## Como fazer:
No Node.js, escrever para stderr pode ser realizado usando o método `console.error()` ou escrevendo diretamente em `process.stderr`. Aqui estão exemplos demonstrando ambas as abordagens:

```javascript
// Usando console.error()
console.error('Esta é uma mensagem de erro.');

// Escrevendo diretamente em process.stderr
process.stderr.write('Esta é outra mensagem de erro.\n');
```

A saída de amostra para ambos os métodos apareceria no fluxo stderr, não se misturando com stdout:
```
Esta é uma mensagem de erro.
Esta é outra mensagem de erro.
```

Para registros mais sofisticados ou específicos da aplicação, muitos programadores JavaScript usam bibliotecas de terceiros como `winston` ou `bunyan`. Aqui está um rápido exemplo usando `winston`:

Primeiro, instale o `winston` via npm:
```shell
npm install winston
```

Em seguida, configure o `winston` para registrar erros em stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Registrando uma mensagem de erro
logger.error('Erro registrado através do winston.');
```

Essa configuração garante que, ao registrar um erro usando o `winston`, ele seja direcionado para stderr, ajudando a manter uma clara separação entre as saídas padrão e de erro.
