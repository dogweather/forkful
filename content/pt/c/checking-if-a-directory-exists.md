---
title:                "Verificar se um diretório existe."
html_title:           "C: Verificar se um diretório existe."
simple_title:         "Verificar se um diretório existe."
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, em nosso programa em C, precisamos verificar se um diretório existe antes de executar certas ações. Isso pode ser útil para garantir que o programa funcione corretamente e evite erros inesperados. Neste artigo, mostraremos como fazer isso de forma simples e eficiente.

## Como fazer

Para verificar se um diretório existe em C, podemos usar a função `opendir()`. Esta função pertence à biblioteca `dirent.h` e nos permite abrir um diretório para leitura. Se o diretório não existir, a função retornará um ponteiro nulo. Isso significa que podemos usá-lo em uma condição para verificar a existência do diretório.

Veja um exemplo de código abaixo:

```
#include <dirent.h>
#include <stdio.h>

int main() {
    char* directory = "meu_diretorio";
    if (opendir(directory) != NULL) {
        printf("O diretório existe!");
    } else {
        printf("O diretório não existe!");
    }
    return 0;
}
```

Neste exemplo, declaramos uma variável `directory` contendo o nome do diretório que queremos verificar. Então, usamos a função `opendir()` para tentar abrir o diretório. Se o retorno da função for diferente de nulo, significa que o diretório existe e imprimimos essa informação na tela.

## Mergulho profundo

Em alguns casos, é necessário verificar se o diretório existe e também se tem permissões de leitura ou escrita. Para isso, podemos usar as funções `access()` e `mkdir()`, respectivamente.

A função `access()` verifica se podemos acessar um determinado caminho de arquivo ou diretório com as permissões fornecidas. Já a função `mkdir()` é usada para criar um novo diretório com as permissões especificadas.

Veja um exemplo abaixo:

```
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

int main() {
    char* directory = "meu_diretorio";
    // verificar se tem permissão de leitura
    if (access(directory, R_OK) != 0) {
        printf("Você não tem permissão para ler este diretório.");
    } else {
        printf("Você pode ler este diretório!");
    }
    // criar um novo diretório com permissão de escrita
    if (mkdir("novo_diretorio", S_IRWXU) == -1) {
        printf("Não foi possível criar o diretório.");
    } else {
        printf("O diretório foi criado com sucesso!");
    }
    return 0;
}
```

Neste exemplo, usamos a função `access()` para verificar se temos permissão de leitura no diretório especificado. Em seguida, usamos `mkdir()` para criar um novo diretório chamado "novo_diretorio" com permissões de leitura, escrita e execução para o usuário atual.

## Veja também

- [Documentação oficial sobre a função opendir()](https://linux.die.net/man/3/opendir)
- [Tutorial sobre o uso de opendir()](https://www.geeksforgeeks.org/c-program-list-files-sub-directories-directory/)
- [Explicação sobre as funções access() e mkdir()](https://www.gnu.org/software/libc/manual/html_node/File-Permission-Bits.html)