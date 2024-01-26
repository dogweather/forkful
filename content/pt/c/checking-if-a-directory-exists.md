---
title:                "Verificando se um diretório existe"
html_title:           "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Checar a existência de um diretório é verificar se uma pasta específica está presente no sistema de arquivos. Programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em diretórios que podem não existir.

## Como fazer:

Para verificar a existência de um diretório em C, podemos usar a função `stat` disponível no cabeçalho `sys/stat.h`. Aqui está um pequeno exemplo:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat statbuf;
    const char *diretorio = "exemplo_diretorio";
    
    // Tenta obter informações sobre o diretório
    if (stat(diretorio, &statbuf) == 0) {
        // Checa se realmente é um diretório
        if (S_ISDIR(statbuf.st_mode)) {
            printf("O diretório '%s' existe.\n", diretorio);
        } else {
            printf("'%s' existe, mas não é um diretório.\n", diretorio);
        }
    } else {
        // O diretório não existe
        printf("O diretório '%s' não existe.\n", diretorio);
    }
    
    return 0;
}
```

Output possível:

```
O diretório 'exemplo_diretorio' existe.
```
ou
```
O diretório 'exemplo_diretorio' não existe.
```

## Aprofundando:

Historicamente, o método de verificação usando `stat` tem sido uma maneira confiável de coletar informações de arquivos e diretórios no sistemas de arquivos POSIX. Existem funções alternativas como `opendir()` e `access()`, cada uma com suas próprias vantagens -- `opendir()` é específica para diretórios e `access()` pode verificar diferentes tipos de permissões. No entanto, uma combinação de `stat` e um teste para `S_ISDIR()` oferece uma abordagem direta e clara para confirmar a existência de diretórios.

Quanto à implementação, a função `stat` preenche uma estrutura `stat` contendo várias informações, incluindo o tipo do arquivo (arquivo regular, diretório, link simbólico, etc.) identificado por `st_mode`. É importante notar que, embora as funções discutidas sejam padrão em ambientes UNIX e similares, abordagens equivalentes podem variar em diferentes sistemas operacionais.

## Veja também:

Para expandir seus conhecimentos, confira os seguintes links:

- Documentação da função `stat`: https://man7.org/linux/man-pages/man2/stat.2.html
- Documentação da função `opendir`: https://man7.org/linux/man-pages/man3/opendir.3.html
- Documentação da função `access`: https://man7.org/linux/man-pages/man2/access.2.html

Esses links direcionam para o manual online 'man pages', que oferece uma visão detalhada das funções disponíveis no sistema Linux, podendo ser similar em outros sistemas UNIX-like.
