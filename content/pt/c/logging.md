---
title:                "Registro de Logs"
date:                  2024-01-26T01:00:01.363807-07:00
model:                 gpt-4-1106-preview
simple_title:         "Registro de Logs"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/logging.md"
---

{{< edit_this_page >}}

## O Quê e Porquê?
Registrar em log é basicamente anotar o que o seu programa está fazendo, tipicamente escrevendo mensagens em um arquivo ou terminal. Programadores fazem isso para acompanhar eventos, diagnosticar problemas e ter um histórico auditável que conta a história da operação de uma aplicação ao longo do tempo.

## Como fazer:
Vamos começar com alguns conceitos básicos. C não possui um framework de registro em log integrado, mas você pode criar algo simples com `stdio.h`. Veja como:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t agora;
    time(&agora);
    char *data = ctime(&agora);
    data[strlen(data) - 1] = '\0'; // Remove a nova linha no final do resultado de ctime()
    printf("[%s] %s\n", data, message);
}

int main() {
    logMessage("A aplicação iniciou.");
    // ... seu código vai aqui ...
    logMessage("A aplicação está fazendo algo importante.");
    // ... seu código continua ...
    logMessage("A aplicação terminou.");
    return 0;
}
```

Um exemplo de saída pode parecer com isso:

```
[Tue Mar 9 12:00:01 2023] A aplicação iniciou.
[Tue Mar 9 12:00:02 2023] A aplicação está fazendo algo importante.
[Tue Mar 9 12:00:03 2023] A aplicação terminou.
```

Claro que, no mundo real, você provavelmente iria querer escrever em um arquivo em vez de um terminal, lidar com diferentes níveis de log e talvez usar uma biblioteca predefinida.

## Aprofundando
Registrar em log em C tem um charme peculiar – é tão baixo nível quanto a maior parte do resto da linguagem. Historicamente, o registro em log era realizado usando `fprintf` com `stderr` ou um ponteiro de arquivo. À medida que os programas se tornavam mais complexos, as necessidades de registro em log também aumentavam, levando ao desenvolvimento de bibliotecas como `syslog` em sistemas Unix, que podiam lidar com logs de múltiplas fontes com vários níveis de importância.

No cenário moderno, existem muitas bibliotecas de registro em log em C disponíveis, como `zlog`, `log4c` e `glog`, que oferecem um conjunto rico de funcionalidades incluindo rotação de logs, registro estruturado e registro multithread. Essas soluções permitem um controle minucioso sobre o verbosidade, destinos e formatos dos logs.

Ao implementar um sistema de registro em log, detalhes como formatação de data e hora, gerenciamento de arquivos de log e desempenho precisam de consideração. Marcar o tempo nos logs é crucial para correlacionar eventos, enquanto a rotação de logs garante que arquivos de log não consumam muito espaço em disco. O ato de registrar em log também deve ser rápido e não bloquear o fluxo principal da aplicação para evitar que o registro em log se torne um gargalo.

## Veja Também
Para mergulhar mais fundo nas bibliotecas e práticas de registro em log em C, confira estes recursos:

- Manual do `syslog` do GNU: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Uma biblioteca de registro em log altamente configurável para C - https://github.com/HardySimpson/zlog
- `log4c`: Um framework de registro em log para C modelado após Log4j - http://log4c.sourceforge.net/
- `glog`: A biblioteca de registro em log de nível de aplicação do Google - https://github.com/google/glog
