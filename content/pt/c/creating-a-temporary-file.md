---
title:                "Criando um arquivo temporário"
html_title:           "C: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar arquivos temporários é uma tarefa comum em programação, pois permite armazenar dados temporariamente durante a execução de um programa. Isso pode ser útil em diversas situações, como gerar relatórios, armazenar caches ou criar arquivos de backup.

## Como Fazer

Para criar um arquivo temporário em C, podemos utilizar a função `tmpfile()` da biblioteca padrão `<stdio.h>`. Essa função irá retornar um ponteiro para um arquivo temporário aberto em modo binário, que poderá ser usado para escrever ou ler dados.

Um exemplo de código seria:

```
#include <stdio.h>

int main() {
  // Criar arquivo temporário
  FILE *arquivo_temporario = tmpfile();

  // Verificar se o arquivo foi criado corretamente
  if (arquivo_temporario == NULL) {
    printf("Erro ao criar arquivo temporário.");
    return 1;
  }

  // Escrever dados no arquivo temporário
  fputs("Esse é um exemplo de arquivo temporário.", arquivo_temporario);

  // Ler dados do arquivo temporário e imprimir na tela
  rewind(arquivo_temporario);
  char buffer[100];
  fgets(buffer, 100, arquivo_temporario);
  printf("%s", buffer);

  // Fechar arquivo temporário
  fclose(arquivo_temporario);

  return 0;
}
```

O código acima irá criar um arquivo temporário, escrever a string fornecida e depois ler e imprimir na tela. Ao final, o arquivo temporário será fechado e excluído automaticamente.

## Deep Dive

Ao usar a função `tmpfile()`, o arquivo temporário criado será automaticamente excluído ao ser fechado. Isso é útil para evitar poluição no sistema com arquivos desnecessários.

Além disso, também é possível criar um arquivo temporário com nome especificado usando a função `tmpnam()`, que irá retornar uma string contendo o caminho para o arquivo temporário criado.

Outra funcionalidade interessante é a possibilidade de definir um diretório específico para criação do arquivo temporário, usando a função `tmpfile_s()` presente na biblioteca `<stdio.h>`.

## Veja Também

- [Documentação Oficial da Função `tmpfile()` em C](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html#Temporary-Files)
- [Exemplo de Uso da Função `tmpfile()` em C](https://www.geeksforgeeks.org/tmpfile-function-in-c-with-examples/)
- [Outras Funções Relacionadas à Criação de Arquivos Temporários](https://www.tutorialspoint.com/c_standard_library/c_function_tmpnam.htm)