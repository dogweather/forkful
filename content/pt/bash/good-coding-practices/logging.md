---
title:                "Registro de Logs"
aliases:
- /pt/bash/logging.md
date:                  2024-01-26T00:59:40.964223-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/logging.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

O registro de logs é a prática de gravar eventos, erros e outras informações significativas dos processos em execução de um programa em um arquivo ou fluxo de saída. Os programadores fazem isso para acompanhar o comportamento de suas aplicações, depurar problemas e manter um registro histórico das operações que podem auxiliar na resolução de problemas futuros.

## Como fazer:

No Bash, registrar logs pode ser tão simples quanto redirecionar ou acrescentar saídas a um arquivo. Aqui está um exemplo básico:

```Bash
echo "Iniciando o script..." >> script.log
# Os comandos do seu script aqui
echo "Script concluído em $(date)" >> script.log
```

Para algo mais avançado, você pode incorporar o syslog para um registro de logs em todo o sistema:

```Bash
logger "Mensagem personalizada do meu script"
```

O `logger` envia uma mensagem de log para o serviço syslog, que então a trata de acordo com a configuração de syslog do sistema.

Exemplo de saída capturada em `script.log`:

```Bash
Iniciando o script...
Script concluído em Ter Mar 23 09:26:35 PDT 2021
```

## Aprofundando

Historicamente, em sistemas semelhantes ao Unix, o registro de logs tem sido facilitado pelo serviço syslog, permitindo que diferentes aplicações e partes do sistema registrem mensagens de forma centralizada. Isso permite a implementação de um mecanismo de registro de logs padronizado em todo o sistema.

Quando se trata de alternativas, alguns podem considerar o uso de `syslog-ng` ou `rsyslog` para recursos de registro de logs mais avançados, ou escrever logs em um banco de dados de séries temporais para fins analíticos. Para aplicações com níveis mais altos de complexidade, pode fazer sentido usar uma biblioteca ou aplicação de registro de logs dedicada como Log4j (no ecossistema Java) ou Monolog (em PHP), que podem fornecer opções de registro estruturadas e configuráveis, mesmo para uma linguagem de script como Bash.

A maneira como você implementa o registro de logs depende muito das necessidades da sua aplicação. Se você só precisa de uma saída simples para acompanhar o progresso do script, acrescentar mensagens a um arquivo é fácil e conveniente. No entanto, para um registro de logs mais escalável e robusto, você vai querer integrar a um sistema de registro que suporte recursos como rotação de logs, níveis de log e registro remoto.

## Veja Também

- As páginas de manual para as funções `logger` e `syslog` são sempre suas amigas, tente `man logger` ou `man syslog`.
- Para um olhar mais aprofundado sobre registro de logs do sistema, considere ler a documentação de `rsyslog` e `syslog-ng`.
- Para descobrir mais sobre o contexto histórico e os princípios por trás do registro de logs em sistemas semelhantes ao Unix, o protocolo `Syslog` documentado na RFC 5424 fornece informações abrangentes.
