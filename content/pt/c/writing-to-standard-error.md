---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever em "standard error" (stderr) é mandar mensagens de erro pra um lugar específico que é diferente da saída normal de um programa. Programadores fazem isso para separar a saída normal da saída de erro, facilitando a vida de quem tá rodando e depurando o programa.

## How to:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Erro encontrad0! \n");
    return 0;
}
```

### Saída de Amostra:
```
Erro encontrado!
```

## Deep Dive
Historicamente, separar stdout (saída padrão) de stderr é uma prática do Unix, adotada para simplificar o processamento de erros. Alternativas incluem o uso de logs ou códigos de retorno. No C, stderr é um FILE * predefinido, acessível sem abrir explicitamente um arquivo. Quando escreve para stderr, o C trata de mandar as mensagens para o lugar certo, mesmo que seja redirecionado em um shell.

## See Also
- Documentação do GNU sobre stderr: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Guia do "Beej's Guide to C Programming" sobre I/O em Arquivos: https://beej.us/guide/bgc/output/html/multipage/stdio.html
- Understanding UNIX/Linux Programming de Bruce Molay para mais contexto histórico.