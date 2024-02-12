---
title:                "Escrevendo no erro padrão"
date:                  2024-02-03T18:15:10.326828-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo no erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Escrever para o erro padrão em C envolve direcionar mensagens de erro e informações de diagnóstico para um fluxo separado da saída principal do programa. Programadores fazem isso para separar as mensagens de erro da saída padrão, tornando ambas mais fáceis de ler e processar separadamente, especialmente quando se está depurando ou registrando a execução de programas.

## Como fazer:

Em C, o fluxo `stderr` é usado para escrever mensagens de erro. Diferentemente de escrever para a saída padrão com `printf`, escrever para `stderr` pode ser feito usando `fprintf` ou `fputs`. Veja como você pode fazer isso:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Esta é uma mensagem de erro.\n");

    fputs("Esta é outra mensagem de erro.\n", stderr);
    
    return 0;
}
```

Saída de exemplo (para stderr):
```
Esta é uma mensagem de erro.
Esta é outra mensagem de erro.
```

É importante notar que, embora a saída pareça semelhante ao `stdout` no console, quando o redirecionamento é usado no terminal, a distinção se torna clara:

```sh
$ ./seu_programa > output.txt
```

Este comando redireciona apenas a saída padrão para `output.txt`, enquanto as mensagens de erro ainda aparecerão na tela.

## Aprofundamento

A distinção entre `stdout` e `stderr` em sistemas baseados em Unix remonta aos primeiros dias de C e Unix. Esta separação permite um manuseio de erros e registro mais robustos, pois permite que programadores redirecionem mensagens de erro independentemente da saída padrão do programa. Enquanto `stderr` é não bufferizado por padrão para garantir a saída imediata de mensagens de erro, o que ajuda na depuração de travamentos e outros problemas críticos, `stdout` é tipicamente bufferizado, o que significa que sua saída pode ser atrasada até que o buffer seja esvaziado (por exemplo, na conclusão do programa ou esvaziamento manual).

Em aplicações modernas, escrever para `stderr` ainda é relevante, especialmente para ferramentas de linha de comando e aplicações de servidor onde distinguir entre mensagens de log regulares e erros é crucial. No entanto, para um manuseio de erros mais complexo, especialmente em aplicações GUI ou onde mecanismos de registro mais sofisticados são necessários, programadores podem usar bibliotecas de registro dedicadas que fornecem mais controle sobre a formatação de mensagens, destinos (por exemplo, arquivos, rede) e níveis de severidade (informação, advertência, erro, etc.).

Embora `stderr` forneça um mecanismo fundamental para relatar erros em C, a evolução das práticas de programação e a disponibilidade de frameworks de registro avançados significam que muitas vezes é apenas o ponto de partida para estratégias modernas de manuseio de erros.