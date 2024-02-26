---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:39.597087-07:00
description: "Registrar eventos, erros ou ocorr\xEAncias not\xE1veis durante a execu\xE7\
  \xE3o de um programa \xE9 o que constitui o logging na programa\xE7\xE3o. Programadores\
  \ fazem isso\u2026"
lastmod: '2024-02-25T18:49:43.778425-07:00'
model: gpt-4-0125-preview
summary: "Registrar eventos, erros ou ocorr\xEAncias not\xE1veis durante a execu\xE7\
  \xE3o de um programa \xE9 o que constitui o logging na programa\xE7\xE3o. Programadores\
  \ fazem isso\u2026"
title: Registro
---

{{< edit_this_page >}}

## O Que & Por Que?

Registrar eventos, erros ou ocorrências notáveis durante a execução de um programa é o que constitui o logging na programação. Programadores fazem isso para depurar problemas, monitorar desempenho e manter um registro de dados operacionais, tornando-o fundamental para manter e compreender o comportamento do software em produção.

## Como Fazer:

No Google Apps Script, o registro pode ser realizado usando vários métodos, como a classe `Logger` e `console.log()`. A classe Logger é a maneira tradicional, adequada para depuração simples e propósitos de desenvolvimento. Com atualizações recentes, `console.log()` oferece mais flexibilidade e integração com o Stackdriver Logging, fornecendo uma solução mais robusta para monitorar seus Apps Scripts na Google Cloud Platform.

**Usando Logger:**

```javascript
function logSample() {
  Logger.log('Esta é uma simples mensagem de log');
  
  var value = 5;
  Logger.log('O valor é: %s', value); // Formatação de string
}

// Para visualizar o log:
// 1. Execute a função logSample.
// 2. Visualizar -> Logs
```

**Saída de Exemplo do Logger:**

```
[22-04-20 10:00:00:000 PDT] Esta é uma simples mensagem de log
[22-04-20 10:00:00:001 PDT] O valor é: 5
```

**Usando console.log():**

```javascript
function consoleLogSample() {
  console.log('Esta mensagem vai para o Stackdriver Logging');
  const obj = {name: 'Jane', role: 'Desenvolvedor'};
  console.info('Registrando um objeto:', obj);
}

// Os logs podem ser visualizados no console do Google Cloud Platform (GCP) sob o Stackdriver Logging
```

**Saída de Exemplo do console.log():**

```
Esta mensagem vai para o Stackdriver Logging
Registrando um objeto: {name: "Jane", role: "Desenvolvedor"}
```

Ao transitar para `console.log()` para aplicações complexas, desenvolvedores podem analisar e parsear logs de maneira eficiente usando os poderosos filtros e ferramentas fornecidos pelo GCP, o que não é tão direto com a classe Logger tradicional.

## Aprofundando:

O registro no Google Apps Script evoluiu significativamente. Inicialmente, a classe `Logger` era o método principal para os desenvolvedores depurarem seus scripts. É simples e suficiente para scripts básicos, mas falta capacidade para aplicações modernas na nuvem, como pesquisar logs ou analisar tendências de logs ao longo do tempo.

A introdução do `console.log()` preencheu essa lacuna integrando o registro do Google Apps Script com o Stackdriver Logging do Google Cloud (agora chamado de Operations Suite), fornecendo uma plataforma centralizada para registro, monitoramento e depuração de aplicações. Isso não apenas permitiu o registro em escala, mas também abriu recursos avançados de gerenciamento de logs, como métricas baseadas em logs, análise de logs em tempo real e integração com outros serviços do Google Cloud.

Embora o `Logger` ainda sirva a um propósito para depuração rápida e registro em scripts menores, a evolução em direção ao uso de `console.log()` reflete uma mudança mais ampla no desenvolvimento de aplicações escaláveis e nativas da nuvem. Isso sublinha o compromisso do Google em fornecer aos desenvolvedores ferramentas que atendam à complexidade e escala das aplicações de hoje. No entanto, os recém-chegados devem estar cientes da curva de aprendizado um pouco mais íngreme e da necessidade de se familiarizar com os conceitos da Google Cloud Platform. Apesar disso, a mudança é vantajosa para os desenvolvedores que buscam aproveitar integralmente as capacidades da nuvem. Esse alinhamento com os serviços de nuvem faz parte de uma tendência mais ampla no desenvolvimento de software, enfatizando a importância de mecanismos de registro robustos e escaláveis na era da computação em nuvem.
