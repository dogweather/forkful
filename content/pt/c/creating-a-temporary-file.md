---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A criação de um arquivo temporário consiste em gerar um arquivo que guarda dados temporariamente durante a execução de um programa. Programadores o fazem quando precisam manipular grandes quantidades de dados que não podem, ou não devem, ser armazenados na memória.

## Como Fazer:

Aqui está um código simples para criar um arquivo temporário usando a biblioteca padrão do C:

```C
#include <stdio.h>

int main()
{
   FILE *tmpfp;
   tmpfp = tmpfile();

   if (tmpfp == NULL) {
      printf("Erro ao abrir o arquivo!\n");
   } else {
      printf("Arquivo temporário criado com sucesso.\n");
   }

   return 0;
}
```

Ao executar este código, o resultado seria:

```C
Arquivo temporário criado com sucesso.
```

## Aprofundando

Derivado das primeiras implementações unix, o uso de arquivos temporários em C tem sido uma prática comum, e necessária, para gerenciamento de recursos. Temos outras alternatuvas disponíveis, tais como o uso de memória virtual, porém, arquivos temporários permitem maior flexibilidade e reduzem o risco de esgotar a memória.

Quanto à implementação, ao chamar a função tmpfile(), um arquivo temporário exclusivo é criado na pasta de arquivos temporários do seu sistema. Este arquivo é então aberto no modo "wb+" sem nenhuma intervenção do usuário. O arquivo temporário é automaticamente apagado quando é fechado (usando fclose()) ou quando o programa termina.

## Veja Também

1. Para aprender mais sobre a função tmpfile() e manipulação de arquivos em C, recomendo o livro "The C Programming Language" de Brian W. Kernighan e Dennis M. Ritchie (https://pt.wikipedia.org/wiki/The_C_Programming_Language)
2. Documentação oficial de C standard library (https://en.cppreference.com/w/cpp/header/cstdio)
3. Tutorial sobre manipulação de arquivos em C (https://www.learn-c.org/en/File_Input_and_Output)