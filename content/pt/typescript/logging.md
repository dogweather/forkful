---
title:                "Registro de Logs"
date:                  2024-01-26T01:08:57.899332-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/logging.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Logging é o processo de registrar eventos, erros e outras informações significativas durante a execução de um programa em um meio externo, frequentemente arquivos ou bancos de dados. Programadores utilizam logs para monitorar o comportamento do software, depurar problemas e rastrear atividades do sistema para análise de segurança e desempenho.

## Como Fazer:

Em TypeScript, você pode facilmente implementar um logging básico usando os métodos do console ou integrar um logging mais avançado com bibliotecas como `winston` ou `pino`. Aqui está um exemplo básico usando `console.log` e um mais avançado com `winston`.

```TypeScript
// Logging básico no console
console.log('Info: Iniciando a aplicação...');
console.error('Erro: Não foi possível recuperar os dados.');

// Saída de Exemplo
// Info: Iniciando a aplicação...
// Erro: Não foi possível recuperar os dados.
```

Para um logging mais robusto, vamos configurar o `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Servidor iniciado!');
logger.warn('Aviso de pouco espaço em disco.');
logger.error('Falha ao conectar ao banco de dados.');

// Saída de Exemplo no combined.log
// 2023-01-20 14:42:07 info: Servidor iniciado!
// 2023-01-20 14:42:09 warn: Aviso de pouco espaço em disco.
// 2023-01-20 14:42:12 error: Falha ao conectar ao banco de dados.
```

## Aprofundamento:

O conceito de logging no contexto da computação remonta aos primeiros dias da programação, onde o termo em si é derivado do "logbook", um sistema de registro usado na navegação. Historicamente, eventos de programas eram frequentemente registrados em impressões físicas ou saídas de terminais, especialmente durante a era dos mainframes.

Avançando para hoje, temos à disposição uma infinidade de ferramentas e bibliotecas que atendem a várias necessidades de logging, desde arquivos de texto simples até sistemas complexos de gerenciamento de logs. Alternativas ao `winston` incluem o `pino`, que se orgulha de alta performance, e o `Bunyan`, baseado em JSON. Ao trabalhar com Node.js, bibliotecas de logging frequentemente fornecem mecanismos de stream para direcionar logs para diferentes destinos, suporte para rotação de logs e formatadores personalizáveis.

Em termos de implementação, mensagens de log geralmente contêm um carimbo de data/hora, um nível de severidade (como info, warn, error) e a mensagem atual. Boas práticas de logging recomendam categorizar os níveis de log adequadamente, evitar dados sensíveis nos logs e considerar as implicações de desempenho em aplicações de alto throughput.

## Veja Também:

- [Winston - Um logger para praticamente tudo](https://www.npmjs.com/package/winston)
- [Pino - Logger Node.js de muito baixo overhead](https://www.npmjs.com/package/pino)
- [Melhores Práticas de Logging em Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [O App de 12 Fatores - Logs](https://12factor.net/logs)