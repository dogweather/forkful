---
title:                "Tratamento de Erros"
date:                  2024-01-26T00:37:08.056294-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de Erros"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/handling-errors.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Tratar erros em C é esperar pelo inesperado. Impede que os programas saiam do controle quando encontram problemas. Os programadores fazem isso para lidar com falhas de maneira elegante e manter seu código confiável.

## Como fazer:

Vamos ver como fazer isso em C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("arquivoinexistente.txt", "r");
    if (fp == NULL) {
        perror("Erro ao abrir arquivo");
        return EXIT_FAILURE;
    }
    // Faz algo com o arquivo
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Saída de exemplo quando o arquivo não existe:
```
Erro ao abrir arquivo: Arquivo ou diretório inexistente
```

## Mergulho Profundo

Nos primórdios de C, o tratamento de erros era básico - principalmente códigos de retorno e checagens manuais. Surge então `errno`, uma variável global atualizada quando funções falham. Não é segura para threads por si só, por isso, as funções mais novas `strerror` e `perror` foram introduzidas para um relatório de erros melhor.

Alternativas? C moderno não está limitado a `errno`. Existe setjmp e longjmp para saltos não-locais quando o desastre acontece. Algumas pessoas preferem definir seus próprios códigos de erro, enquanto outras optam por estruturas semelhantes a exceções em C++.

Os detalhes de implementação podem ser complexos. Por exemplo, `errno` é seguro para threads em sistemas compatíveis com POSIX devido à mágica do Armazenamento Local de Threads (TLS). Em sistemas embarcados, onde os recursos são preciosos, pode-se preferir código de tratamento de erros personalizado em vez de abordagens padrão que podem inflar o software.

## Veja Também

- Um mergulho detalhado em `errno`: https://en.cppreference.com/w/c/error/errno
- Para segurança em threads, veja POSIX threads e errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Uma introdução a setjmp e longjmp: https://www.cplusplus.com/reference/csetjmp/
- Para tratamento de exceção em C++, consulte: https://isocpp.org/wiki/faq/exceptions
