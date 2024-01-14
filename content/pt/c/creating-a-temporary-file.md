---
title:                "C: Criando um arquivo temporário"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em programação?

Criar um arquivo temporário em programação pode ser útil em diversas situações, como para armazenar dados temporários que serão usados posteriormente ou para criar backups antes de modificar um arquivo de dados original. No entanto, é importante lembrar que esses arquivos são temporários e devem ser excluídos após o uso para evitar ocupar espaço desnecessário no disco.

## Como criar um arquivo temporário em C?

A criação de um arquivo temporário em C é relativamente simples e pode ser feita utilizando a função `tmpfile ()`. Esta função cria um arquivo temporário vazio e retorna um ponteiro para ele. Veja um exemplo de código abaixo:

```C
#include<stdio.h>
#include<stdlib.h>

int main() {
    // Criando um arquivo temporário
    FILE *temp_file = tmpfile();

    if (temp_file == NULL) {
        printf("Erro ao criar arquivo temporário!");
        return 1;
    }

    // Escrevendo dados no arquivo temporário
    fprintf(temp_file, "Este é um exemplo de arquivo temporário criado em C.\n");

    // Lendo dados do arquivo temporário
    rewind(temp_file);

    char buffer[100];

    fgets(buffer, 100, temp_file);
    printf("Conteúdo do arquivo temporário: %s", buffer);

    // Limpando e excluindo o arquivo temporário
    fclose(temp_file);
    remove("temp_file");

    return 0;
}
```

A saída deste código seria: `Conteúdo do arquivo temporário: Este é um exemplo de arquivo temporário criado em C.`.

## Aprofundando na criação de arquivos temporários

Além da função `tmpfile()`, existem outras formas de criar arquivos temporários em C. Por exemplo, a função `tmpnam()` cria um nome para um arquivo temporário e retorna uma string contendo o caminho para este arquivo. Outra opção é utilizar a biblioteca `stdlib.h` para criar um arquivo temporário com `mkstemp()`, que permite criar o arquivo com um nome específico.

É importante lembrar que, ao criar um arquivo temporário, é necessário garantir que ele seja excluído após o uso para evitar possíveis problemas de segurança ou ocupação desnecessária de espaço no disco.

## Veja também

- [Função `tmpfile()` em C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Manipulação de arquivos em C](https://www.embarcados.com.br/manipulacao-de-arquivos-em-c/)
- [Criação de arquivos temporários em diferentes sistemas operacionais](https://stackoverflow.com/questions/9403250/create-a-temporary-file-in-c)