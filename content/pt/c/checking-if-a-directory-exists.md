---
title:    "C: Verificando se um diretório existe."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Quando estamos escrevendo programas em C, muitas vezes precisamos acessar e manipular arquivos e diretórios. Saber se um determinado diretório existe é importante porque nos permite realizar ações específicas, como criar novos diretórios, copiar arquivos ou até mesmo evitar erros em nossos programas. Portanto, é essencial saber como verificar se um diretório existe em nossos programas em C.

## Como Fazer

Antes de tudo, precisamos incluir a biblioteca `stdio.h`, que contém funções de entrada e saída, e a biblioteca `stdbool.h`, que nos dá acesso ao tipo de dado `bool`. Em seguida, podemos utilizar a função `opendir` para abrir um diretório e armazenar seu ponteiro em uma variável.

```C
#include <stdio.h>
#include <stdbool.h>

int main() {
    // Abre o diretório "exemplo"
    DIR* dir = opendir("exemplo");
    // Verifica se o diretório foi aberto com sucesso
    if (dir) { 
        printf("O diretório existe!");
        // Fecha o diretório
        closedir(dir);
    } else {
        printf("O diretório não existe!");
    }
    return 0;
}
```

O código acima verifica se o diretório "exemplo" existe e imprime uma mensagem correspondente. Se o diretório existir, seu ponteiro é armazenado na variável `dir`, caso contrário, a variável é nula.

## Mergulho Profundo

Além da função `opendir`, existem outras maneiras de verificar se um diretório existe. Uma opção é utilizar a função `chdir`, que muda o diretório atual para um especificado e retorna 0 se for bem sucedida e -1 se falhar. Outra opção é usar a função `access`, que verifica se um certo caminho de arquivo é acessível e retorna 0 se for bem sucedida e -1 se falhar. Essas funções também podem ser úteis para verificar a existência de arquivos, não apenas diretórios.

## Veja Também

- [Manual de Referência da Biblioteca `stdio.h`](https://pt.wikipedia.org/wiki/Stdio.h)
- [Manual de Referência da Biblioteca `stdbool.h`](https://pt.wikipedia.org/wiki/Stdbool.h)
- [Documentação da Função `opendir`](https://pubs.opengroup.org/onlinepubs/009695399/functions/opendir.html)
- [Documentação da Função `chdir`](https://pubs.opengroup.org/onlinepubs/009695399/functions/chdir.html)
- [Documentação da Função `access`](https://pubs.opengroup.org/onlinepubs/009695399/functions/access.html)