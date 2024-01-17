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

## O que & Por quê?
Criar um arquivo temporário é uma tarefa comum realizada por programadores em C. Isso porque, ao criar um arquivo temporário, é possível armazenar dados temporários que serão usados durante a execução do programa e que não precisam ser armazenados permanentemente no sistema.

## Como fazer:
Para criar um arquivo temporário em C, é necessário utilizar a função `tmpfile()`. Ela criará um arquivo temporário e retornará um ponteiro para o mesmo. A seguir, um exemplo prático:

```
#include <stdio.h>

int main()
{
    FILE * fp; //ponteiro para o arquivo temporário
    char c; //variável para armazenar o conteúdo do arquivo
    
    fp = tmpfile(); //criando o arquivo temporário
    
    //verificando se o arquivo foi criado corretamente
    if(fp != NULL)
    {
        fputs("Esse é um arquivo temporário criado em C.", fp); //escrevendo no arquivo
        rewind(fp); //retornando ao começo do arquivo
        c = fgetc(fp); //lendo um caractere do arquivo
        printf("Conteúdo do arquivo: %c", c); //mostrando o conteúdo do arquivo
        fclose(fp); //fechando o arquivo
    }
    else //caso ocorra algum erro
    {
        printf("Não foi possível criar o arquivo temporário.");
    }

    return 0;
}
```
**Output:**
```
Conteúdo do arquivo: E
```

## Profundando:
A criação de arquivos temporários é uma técnica muito antiga e é amplamente utilizada em diferentes linguagens de programação. Além da função `tmpfile()`, também é possível criar um arquivo temporário utilizando as funções `fopen()` e `mkstemp()`. No entanto, a função `tmpfile()` é considerada mais segura, pois o arquivo criado é automaticamente removido quando o programa é finalizado, evitando a poluição do sistema com arquivos inúteis.

É importante ressaltar que o uso de arquivos temporários pode ser uma vulnerabilidade de segurança, pois eles podem ser acessados e modificados por outros programas. Portanto, é fundamental ter cuidado ao usar esse recurso e sempre excluir o arquivo temporário após o seu uso.

## Veja também:
- [Documentação oficial da função `tmpfile()` em C](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Diferença entre arquivos temporários e permanentes](https://pt.stackoverflow.com/questions/50216/qual-%C3%A9-a-diferen%C3%A7a-entre-arquivo-tempor%C3%A1rio-e-arquivo-permanente)