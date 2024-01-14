---
title:                "C: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Porque verificar se um diretório existe

Se você é um programador experiente em C, provavelmente já se deparou com a necessidade de verificar se um diretório existe antes de realizar alguma operação. Isso é especialmente importante se o seu programa depende de arquivos que devem estar presentes em um determinado diretório. Neste artigo, vamos explorar como verificar a existência de um diretório em um programa escrito em linguagem C.

## Como verificar se um diretório existe em C

Em C, existem duas maneiras de verificar se um diretório existe: utilizando a função `opendir()` ou `access()`. Ambas as funções são definidas na biblioteca padrão `dirent.h`.

### Utilizando `opendir()`

A função `opendir()` é utilizada para abrir um diretório e, caso seja bem-sucedida, retorna um ponteiro para a sua estrutura `DIR`. Se o diretório não existir, a função retornará `NULL`.

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("/home/usuario/Documents"); // diretório existente
    if(dir) {
        // o diretório existe
        printf("O diretório existe\n");
    }
    else {
        // o diretório não existe
        printf("O diretório não existe\n");
    }
    
    return 0;
}
```

O código acima tenta abrir o diretório `/home/usuario/Documents`, que é um diretório existente no sistema. Se compilarmos e executarmos o programa, o output será:

```
O diretório existe
```

Mas o que acontece se o diretório não existir? Vamos modificar o código para tentar abrir o diretório `/home/usuario/Fotos`, que não existe em nosso sistema:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    DIR *dir = opendir("/home/usuario/Fotos"); // diretório não existente
    if(dir) {
        // o diretório existe
        printf("O diretório existe\n");
    }
    else {
        // o diretório não existe
        printf("O diretório não existe\n");
    }
    
    return 0;
}
```

Ao executar o programa, o output será:

```
O diretório não existe
```

### Utilizando `access()`

A função `access()` é utilizada para verificar se o programa tem permissão de acesso a um determinado arquivo ou diretório. Se o arquivo ou diretório existir, a função retornará `0`; caso contrário, retornará `-1`.

```C
#include <stdio.h>
#include <unistd.h>

int main() {
    int dir = access("/home/usuario/Documents", F_OK); // diretório existente
    if(!dir) {
        // o diretório existe
        printf("O diretório existe\n");
    }
    else {
        // o diretório não existe
        printf("O diretório não existe\n");
    }
    
    return 0;
}
```

O argumento `F_OK` especifica que queremos verificar apenas a existência do diretório. Se compilarmos e executarmos o programa, o output será o mesmo que o do exemplo anterior:

```
O diretório existe
```

Caso o diretório não exista, o output será:

```
O diretório não existe
```

## Aprofundando

Ambas as funções apresentadas são úteis para verificar a existência de um diretório. No entanto, a função `access()` é mais abrangente, pois também pode ser utilizada para verificar a existência de arquivos e verificar permissões de acesso. Além disso, ela é compatível com as versões mais recentes do C, enquanto a função `opendir()` é uma função legada e pode não estar disponível em todos os sistemas operacionais.

## Veja também

- [Documentação da função `opendir()` (em inglês)](https://www.gnu.org/software/libc/manual/html_node/List-of-Directory-Stream-Functions.html)
- [Documentação da função `access()` (em inglês)](https://www.gnu.org/software/libc/manual/html_node/File-Access-Permission.html)