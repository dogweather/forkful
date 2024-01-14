---
title:    "C: Verificando se um diretório existe"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Quando se está trabalhando com programação, é comum encontrar situações em que é necessário verificar se um diretório existe ou não. Isso pode ser útil para controlar o fluxo do programa, evitar erros como tentar acessar um diretório inexistente e oferecer uma melhor experiência ao usuário.

## Como fazer isso em C

Para verificar se um diretório existe em C, é necessário utilizar a função `opendir()` da biblioteca `<dirent.h>`. Esta função retorna um ponteiro para o diretório se for encontrado, ou `NULL` se não existir.

Veja um exemplo de código para verificar se o diretório "documents" existe:

```C
#include <stdio.h>
#include <dirent.h>

int main() {
    // tenta abrir o diretório "documents"
    if (opendir("documents") != NULL) {
        printf("O diretório existe! :)\n");
    } else {
        printf("O diretório não existe :(\n");
    }
    return 0;
}
```

Ao executar este código, se o diretório "documents" existir, será exibida a mensagem "O diretório existe! :)", caso contrário, será exibida a mensagem "O diretório não existe :(".

## Profundando no assunto

A função `opendir()` não é a única maneira de verificar se um diretório existe em C. Outra opção é utilizar a função `stat()` da biblioteca `<unistd.h>`, que também pode ser usada para verificar a existência de arquivos.

Ambas as funções possuem vantagens e desvantagens, sendo a função `opendir()` mais indicada para casos em que é necessário percorrer o diretório, enquanto a função `stat()` é mais eficiente para verificar apenas a existência de um único arquivo ou diretório.

## Veja também

- [Documentação oficial do C sobre a função opendir()](https://www.gnu.org/software/libc/manual/html_node/Opening-a-Directory.html)
- [Documentação oficial do C sobre a função stat()](https://www.gnu.org/software/libc/manual/html_node/File-Attributes.html#File-Attributes)
- [Comparação entre as funções opendir() e stat()](https://stackoverflow.com/questions/7618409/directory-exists-and-file-exists)