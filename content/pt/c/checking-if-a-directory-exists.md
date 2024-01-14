---
title:                "C: Verificando se um diretório existe"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que checar se um diretório existe?

Ao programar em C, muitas vezes nos deparamos com cenários em que precisamos verificar a existência de um diretório. Isso pode ser útil em situações como garantir que o caminho especificado pelo usuário seja válido antes de prosseguir com a execução do programa. Portanto, é importante saber como verificar se um diretório existe em C.

## Como fazer

Para verificar se um diretório existe em C, podemos utilizar a função `opendir()` da biblioteca padrão `dirent.h`. Esta função retorna um ponteiro para o diretório especificado e, se o ponteiro for nulo, significa que o diretório não existe. Segue abaixo um exemplo de código que utiliza esta função:

```C
#include <stdio.h>
#include <dirent.h>

int main()
{
    DIR *dir = opendir("Caminho/para/diretorio");

    if (dir == NULL) {
        // Diretório não existe
        printf("O diretório não existe!\n");
    } else {
        // Diretório existe
        printf("O diretório existe!\n");
        closedir(dir);
    }

    return 0;
}
```

Ao executar este código, se o diretório especificado existir, a saída será `O diretório existe!`, caso contrário, a saída será `O diretório não existe!`.

## Explorando mais a fundo

Além da função `opendir()`, também podemos utilizar a função `stat()` da biblioteca padrão `sys/stat.h` para verificar a existência de um diretório em C. Esta função retorna uma estrutura `struct stat` com informações sobre o arquivo ou diretório especificado. No entanto, no caso de um diretório, é necessário utilizar a macro `S_ISDIR()` para verificar se o arquivo é, de fato, um diretório. Segue abaixo um exemplo de código que utiliza esta função:

```C
#include <stdio.h>
#include <sys/stat.h>

int main()
{
    struct stat file;

    if (stat("Caminho/para/diretorio", &file) == 0) {
        // Diretório existe
        if (S_ISDIR(file.st_mode)) {
            printf("O arquivo especificado é um diretório!\n");
        } else {
            printf("O arquivo especificado não é um diretório!\n");
        }
    } else {
        // Diretório não existe
        printf("O diretório não existe!\n");
    }

    return 0;
}
```

Ao executar este código, se o diretório especificado existir, a saída será `O arquivo especificado é um diretório!`, caso contrário, a saída será `O diretório não existe!`.

## Veja também

- [Documentação da função `opendir()` (em inglês)](https://pubs.opengroup.org/onlinepubs/009696699/functions/opendir.html)
- [Documentação da função `stat()` (em inglês)](https://pubs.opengroup.org/onlinepubs/009696699/functions/stat.html)