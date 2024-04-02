---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:21.840341-07:00
description: "Obter a data atual no Google Apps Script \xE9 sobre buscar a data e\
  \ hora ao vivo, uma tarefa comum para automatizar tarefas, fazer registros e marcar\u2026"
lastmod: '2024-03-13T22:44:46.119946-06:00'
model: gpt-4-0125-preview
summary: "Obter a data atual no Google Apps Script \xE9 sobre buscar a data e hora\
  \ ao vivo, uma tarefa comum para automatizar tarefas, fazer registros e marcar\u2026"
title: Obtendo a data atual
weight: 29
---

## O Que & Por Que?

Obter a data atual no Google Apps Script é sobre buscar a data e hora ao vivo, uma tarefa comum para automatizar tarefas, fazer registros e marcar timestamps em aplicativos ligados ao ecossistema do Google. Programadores usam isso para a geração de conteúdo dinâmico, acompanhamento de prazos e agendamento dentro de Google Docs, Sheets e outros serviços do Google.

## Como fazer:

O Google Apps Script, que é baseado em JavaScript, oferece métodos simples para obter a data atual. Você pode usar o construtor `new Date()` para criar um novo objeto de data representando a data e hora atuais. Veja como você pode manipular e exibir isso em vários formatos.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Registra a data e hora atuais no fuso horário do script
  
  // Para exibir apenas a data no formato AAAA-MM-DD
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Exemplo de saída: "2023-04-01"
  
  // Exibindo em um formato mais legível
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Exemplo de saída: "1 de abril de 2023, 12:00:00 PM GMT+1"
}
```

Esses trechos demonstram como capturar e formatar a data e hora atuais, mostrando versatilidade para diversas necessidades de programação dentro do Google Apps Script.

## Aprofundamento

Antes de o JavaScript ter se decidido pelo objeto `Date`, os programadores tinham que manualmente acompanhar o tempo e a data por meios menos padronizados e mais incômodos. Isso incluía o uso de inteiros de timestamp e funções de data caseiras, que variavam de um ambiente de programação para outro, levando a inconsistências e problemas de compatibilidade.

A introdução do objeto `new Date()` no JavaScript, e por extensão no Google Apps Script, padronizou as operações de data e hora, tornando-as mais intuitivas e reduzindo a quantidade de código necessário para operações relacionadas a data. Vale notar que, embora a implementação do Google Apps Script seja conveniente e suficiente para muitas aplicações dentro da suíte de produtos do Google, ela pode não atender a todos os cenários, especialmente aqueles que requerem um manuseio de fuso horário complexo ou registro preciso de timestamps em ambientes dinâmicos.

Para tais casos de uso avançados, os programadores muitas vezes recorrem a bibliotecas como Moment.js ou date-fns em JavaScript. Embora o Google Apps Script não suporte nativamente essas bibliotecas, os desenvolvedores podem imitar algumas de suas funcionalidades usando os métodos de Data disponíveis em JavaScript ou acessando bibliotecas externas através do HTML Service ou do serviço de busca de URL do Apps Script. Apesar dessas alternativas, a simplicidade e integração das funções de data e hora nativas do Google Apps Script permanecem como a solução preferida para a maioria das tarefas no ecossistema do Google.
