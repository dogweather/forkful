---
date: 2024-01-26 01:07:43.561422-07:00
description: "O logging \xE9 o processo de registrar eventos da aplica\xE7\xE3o enquanto\
  \ um programa est\xE1 em execu\xE7\xE3o, fornecendo um rastro de migalhas para an\xE1\
  lise post-mortem\u2026"
lastmod: '2024-03-13T22:44:46.160678-06:00'
model: gpt-4-1106-preview
summary: "O logging \xE9 o processo de registrar eventos da aplica\xE7\xE3o enquanto\
  \ um programa est\xE1 em execu\xE7\xE3o, fornecendo um rastro de migalhas para an\xE1\
  lise post-mortem e monitoramento em tempo real."
title: Registro de Logs
weight: 17
---

## O Que & Por Quê?
O logging é o processo de registrar eventos da aplicação enquanto um programa está em execução, fornecendo um rastro de migalhas para análise post-mortem e monitoramento em tempo real. Os programadores fazem isso porque ajuda a depurar problemas, monitorar o desempenho e rastrear ações dos usuários para segurança e análise.

## Como Fazer:
O Python vem com um módulo embutido para logging. Aqui está uma configuração básica:
```Python
import logging

# Configuração básica do logging
logging.basicConfig(level=logging.INFO)

# Mensagens de logging
logging.debug('Esta é uma mensagem de debug')
logging.info('Informações sobre o que o seu programa acabou de fazer')
logging.warning('Uma mensagem de alerta')
logging.error('Ocorreu um erro')
logging.critical('O programa não consegue se recuperar!')
```
Quando você executar este código, verá a seguinte saída (já que o nível default é WARNING, mensagens de debug e info não serão mostradas):
```
WARNING:root:Uma mensagem de alerta
ERROR:root:Ocorreu um erro
CRITICAL:root:O programa não consegue se recuperar!
```
Você também pode configurar o logging para escrever em um arquivo em vez de no console:
```Python
logging.basicConfig(filename='app.log', filemode='w', level=logging.INFO)
```
Agora seus logs serão direcionados para o arquivo 'app.log'.

## Aprofundamento
O logging existe desde os primeiros dias de programação, com os logs do sistema sendo uma das formas mais antigas de armazenamento persistente fora dos arquivos reais que contêm dados. História à parte, o conceito principal de logging permanece essencialmente inalterado, embora as ferramentas tenham evoluído.

O módulo `logging` do Python é bastante poderoso e flexível. Ele permite que os programadores definam diferentes níveis de log (DEBUG, INFO, WARNING, ERROR, CRITICAL) que podem ajudar na categorização e filtragem dos logs. Tem um sistema de loggers hierárquico, o que significa que você pode ter relações de parentesco entre loggers e propagar mensagens na cadeia.

Alternativas incluem bibliotecas de terceiros como Loguru ou structlog que oferecem recursos aprimorados e uma interface mais simples do que o módulo de logging embutido. Eles podem fornecer saídas mais bonitas, melhor serialização de dados estruturados e maneiras mais intuitivas de lidar com a configuração de logs.

Em relação à implementação, ao configurar o logging, é importante fazê-lo uma vez no início da sua aplicação. Configurá-lo no nível do módulo é recomendado, usando `logging.getLogger(__name__)` para seguir as melhores práticas de logging do Python.

O logging não deve afetar drasticamente o desempenho de uma aplicação em circunstâncias normais. No entanto, deve-se tomar cuidado com o que é registrado: um logging excessivamente verboso, especialmente em níveis DEBUG, pode desacelerar uma aplicação e rapidamente encher o armazenamento do arquivo de log.

## Veja Também
Para mais informações sobre o módulo de logging do Python, confira o cookbook oficial de logging do Python para alguns ótimos exemplos e melhores práticas: https://docs.python.org/3/howto/logging-cookbook.html

Para um olhar aprofundado sobre logging estruturado e como ele pode ajudar a tornar os logs mais informativos e mais fáceis de analisar, o Loguru está bem documentado: https://loguru.readthedocs.io

Além disso, considere dar uma olhada na metodologia da aplicação 12-fatores, especificamente a seção sobre logs para uma visão moderna sobre logging em aplicativos: https://12factor.net/logs
