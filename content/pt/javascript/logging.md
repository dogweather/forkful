---
title:                "Registro de Logs"
date:                  2024-01-26T01:06:31.454366-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/logging.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
O log, em resumo, é como manter um diário para sua aplicação — ele registra eventos, erros e outras ações significativas que ocorrem enquanto o software está em execução. Os programadores o fazem não apenas para compreender o que está acontecendo por trás dos bastidores em tempo real, mas também para ter um registro histórico que é crucial para depuração, auditoria e otimização de desempenho.

## Como Fazer:
Por padrão, o JavaScript oferece uma maneira simples de registrar mensagens no console:

```javascript
console.log('Isso será registrado no console');

// Saída:
// Isso será registrado no console
```

Mas aplicativos do mundo real exigem mais do que apenas imprimir mensagens no console. Bibliotecas como Winston ou Pino podem ser introduzidas para gerenciar logs de maneira eficaz:

```javascript
// Usando Winston para logging avançado
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Olá, este é um evento de registro com Winston');
// Este log é escrito no arquivo 'combined.log' em formato JSON
```

Exemplo de saída de `combined.log`:

```json
{"message":"Olá, este é um evento de registro com Winston","level":"info"}
```

## Mergulho Profundo
O log é essencial desde os primeiros dias da computação; operadores de sistemas percorriam os logs para entender o desempenho do sistema e diagnosticar problemas. Avançando para o desenvolvimento moderno, passamos de arquivos de log simples para sistemas de gerenciamento de log estruturado e pesquisável.

Alternativas para o log no console ou baseado em arquivos em JavaScript incluem a utilização de serviços de log baseados na nuvem, como Loggly, Datadog ou ELK Stack (Elasticsearch, Logstash, Kibana), que podem agregar logs de várias fontes, oferecer ferramentas de visualização e análises avançadas.

Ao implementar o log, considere o seguinte:
- **Nível de Detalhe**: Incluindo debug, info, warning, error e critical.
- **Desempenho**: Um log excessivo pode afetar o desempenho da aplicação.
- **Segurança**: Tenha cuidado ao registrar informações sensíveis.
- **Formato**: Logs estruturados (como JSON) facilitam a pesquisa e análise dos logs.
- **Políticas de Retenção**: Logs antigos precisam ser arquivados ou excluídos para economizar espaço.

Uma estratégia de log prática define o que registrar, onde registrar e por quanto tempo manter, equilibrando informações úteis contra considerações de desempenho e privacidade.

## Veja Também
Confira estes recursos para um mergulho mais profundo:
- [Repositório GitHub do Winston](https://github.com/winstonjs/winston): para uso detalhado e transports personalizados.
- [Pino - Logger Node.js de muito baixo overhead](https://github.com/pinojs/pino): uma solução de log leve.
- [MDN Web Docs: Console](https://developer.mozilla.org/en-US/docs/Web/API/Console): para informações básicas de log baseado no navegador.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): um trio poderoso para gerenciar logs.
- [12 Factor App Logging](https://12factor.net/logs): melhores práticas em log de aplicações.
