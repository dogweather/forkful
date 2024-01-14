---
title:    "C: Criando um arquivo temporário"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em C?

Criar um arquivo temporário em C pode ser útil quando precisamos armazenar dados temporariamente durante a execução de um programa. Isso pode ser utilizado, por exemplo, para armazenar dados de cache, gerar arquivos de log ou salvar informações temporárias para serem acessadas posteriormente. Além disso, os arquivos temporários são excluídos automaticamente após a execução do programa, poupando espaço em nosso sistema.

## Como criar um arquivo temporário em C

Para criar um arquivo temporário em C, podemos utilizar a função `tmpfile()`, que retorna um ponteiro para o arquivo temporário criado. Em seguida, podemos utilizar as funções de entrada e saída de arquivo, como `fprintf()` e `fscanf()`, para escrever e ler dados no arquivo. Veja um exemplo abaixo:

```c
#include <stdio.h>

int main() {
    // Criando o arquivo temporário
    FILE *tempfile = tmpfile();

    // Escrevendo dados no arquivo
    fprintf(tempfile, "Olá, mundo!");

    // Lendo os dados do arquivo
    char buffer[50];
    fscanf(tempfile, "%s", buffer);

    // Imprimindo os dados
    printf("Conteúdo do arquivo temporário: %s", buffer);

    // Fechando o arquivo
    fclose(tempfile);

    return 0;
}
```

Output:

```bash
Conteúdo do arquivo temporário: Olá, mundo!
```

## Aprofundando-se na criação de um arquivo temporário em C

Ao criar um arquivo temporário em C, é importante lembrar que, por padrão, ele será criado dentro da pasta de diretório temporário do sistema operacional. No entanto, se desejarmos especificar um diretório diferente, podemos utilizar a função `tmpnam()`. Além disso, podemos utilizar a função `mkstemp()` para criar um arquivo temporário de forma mais segura, pois ela nos retorna o descritor de arquivo do arquivo criado, evitando possíveis conflitos.

## Veja também

- [Tutorial completo sobre criação de arquivos em C](https://www.devmedia.com.br/criando-arquivos-usando-stdio-h/9537)
- [Documentação oficial sobre a função `tmpfile()`](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Outras funções de entrada e saída de arquivo em C](https://docs.microsoft.com/pt-br/cpp/c-runtime-library/reference/fctempls-s-fctemplates?view=msvc-160)