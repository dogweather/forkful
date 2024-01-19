---
title:                "Verificando se um diretório existe"
html_title:           "C: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Por Quê? 
Verificar se um diretório existe é uma função simples que retorna um valor verdadeiro ou falso dependendo da existência do diretório especificado. Programadores fazem isso para evitar erros - tentar acessar ou manipular um diretório que não existe pode resultar em erros ou comportamento inesperado do programa.

## Como Fazer:
Usamos a função `stat` em C para verificar se um diretório existe. Segue abaixo um exemplo de seu uso.

```C
#include <sys/stat.h>
#include <stdbool.h>

bool DiretorioExiste(const char* dir_path) {
    struct stat buffer;
    return (stat(dir_path, &buffer) == 0);
}

int main() {
    if(DiretorioExiste("/caminho/para/verificar")) {
        printf("O diretório existe!\n");
    } else {
        printf("O diretório não existe!\n");
    }
    return 0;
}
```

## Mergulhando Fundo
Historicamente, a função `stat` vem da era Unix, desenvolvida na década de 70. Ela é amplamente utilizada para verificar detalhes do sistema de arquivos, incluindo a verificação de diretórios. 

Uma alternativa para a função `stat` é a função `opendir`, que também pode ser usada para verificar se um diretório existe. No entanto, `opendir` tentará realmente abrir o diretório, o que pode ser mais lento que `stat`, que apenas confere as informações sobre o diretório.

Em termos de implementação, a função `stat` simplesmente preenche uma estrutura `struct stat` com informações sobre o arquivo/diretório. Se o arquivo/diretório não existir, `stat` retornará -1, permitindo assim a verificação da existência do diretório.

## Veja Também
1. Mais sobre a função `stat`: [https://man7.org/linux/man-pages/man2/stat.2.html](https://man7.org/linux/man-pages/man2/stat.2.html)
2. Sobre a função `opendir`: [https://man7.org/linux/man-pages/man3/opendir.3.html](https://man7.org/linux/man-pages/man3/opendir.3.html)
3. Documentação completa de referência da biblioteca C: [http://www.cplusplus.com/reference/clibrary/](http://www.cplusplus.com/reference/clibrary/)