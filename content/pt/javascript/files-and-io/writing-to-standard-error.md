---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:34.944144-07:00
description: "Como fazer: No Node.js, escrever para stderr pode ser realizado usando\
  \ o m\xE9todo `console.error()` ou escrevendo diretamente em `process.stderr`. Aqui\u2026"
lastmod: '2024-03-13T22:44:46.978538-06:00'
model: gpt-4-0125-preview
summary: "No Node.js, escrever para stderr pode ser realizado usando o m\xE9todo `console.error()`\
  \ ou escrevendo diretamente em `process.stderr`."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

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
