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

# O que é e por que fazer? 

Verificar se um diretório existe é verificar se determinado caminho de arquivo está acessível no sistema. Programadores geralmente fazem essa checagem para garantir que o programa não tente acessar ou salvar arquivos em diretórios inexistentes, evitando erros e instabilidades no sistema.

# Como fazer:

Para verificar se um diretório existe em um programa escrito em C, podemos utilizar a função `opendir()` da biblioteca `<dirent.h>`. Veja o exemplo abaixo:

```C
#include <stdio.h>
#include <dirent.h>

int main(){
    // Note que "diretorio" é o caminho para o diretório a ser checado
    DIR *dir = opendir("diretorio");

    if (dir){ // Se o diretório foi aberto sem problemas
        printf("O diretório existe!\n");
        closedir(dir); // Fecha o diretório
    }
    else{ // Se o diretório não existe ou ocorreu algum erro ao abri-lo
        printf("O diretório não existe!\n");
    }

    return 0;
}
```

A saída do programa será "O diretório existe!" caso o diretório exista no sistema, ou "O diretório não existe!" caso contrário.

# Profundando:

Uma das principais razões para programadores checarem se um diretório existe é justamente prevenir problemas no acesso a arquivos e pastas inexistentes. Além disso, essa prática também ajuda a garantir a estabilidade do programa e uma melhor gestão de erros.

Uma alternativa para a utilização da função `opendir()` é a função `access()` da biblioteca `<unistd.h>`, que também pode ser usada para verificar a existência de arquivos e diretórios em C.

No sistema de arquivos do Unix, há a convenção de que se um diretório existir, sua permissão de leitura será negada caso não haja permissão de execução. Ou seja, se o usuário não tiver permissão de execução para um diretório, a função `access()` irá retornar -1, indicando que o diretório não existe. Porém, essa convenção pode variar de sistema para sistema, portanto é importante estar atento às especificidades do seu sistema operacional.

Por fim, vale lembrar que a função `opendir()` também pode aceitar um parâmetro adicional, que é o caminho para um arquivo dentro do diretório a ser verificado. Dessa forma, podemos checar a existência de um arquivo específico dentro de um diretório.

# Veja também:

- [Documentação da função `opendir()` no site cppreference](https://en.cppreference.com/w/c/io/opendir)
- [Documentação da função `access()` no site cppreference](https://en.cppreference.com/w/c/io/access)