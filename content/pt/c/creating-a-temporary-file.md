---
title:                "C: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por que criar um arquivo temporário em programação?

Ao escrever código em linguagem C, é comum a necessidade de criar um arquivo temporário. Um arquivo temporário é um arquivo criado pelo programa durante a execução, geralmente para armazenar informações temporárias que serão descartadas após o uso.

## Como criar um arquivo temporário em C?

Para criar um arquivo temporário em C, podemos usar a função `tmpfile()`. Veja abaixo um exemplo de código e a saída correspondente:

```C
// Cria um arquivo temporário
FILE *temp = tmpfile();

// Escreve informações no arquivo
fprintf(temp, "Este é um arquivo temporário criado pelo programa.");

// Lê e imprime o conteúdo do arquivo
char buffer[50];
fscanf(temp, "%s", buffer);
printf("%s", buffer);

// Fecha o arquivo temporário
fclose(temp);
```

Saída:

```
Este é um arquivo temporário criado pelo programa.
```

## Aprofundando no assunto

Ao chamar a função `tmpfile()`, um arquivo temporário vazio é criado no diretório temporário do sistema. Podemos também especificar um caminho para a criação do arquivo, usando a função `tmpnam()`. 

É importante lembrar que um arquivo temporário é automaticamente removido após o encerramento do programa. Porém, se for necessário mantê-lo, podemos renomeá-lo usando a função `rename()`.

Existem diversas situações em que a criação de um arquivo temporário é útil, como por exemplo quando precisamos armazenar grandes quantidades de dados durante a execução de um programa ou quando precisamos realizar operações com arquivos de forma segura, sem correr o risco de sobrescrever arquivos já existentes.

# Veja também

- Documentação da função `tmpfile()`: https://linux.die.net/man/3/tmpfile
- Tutorial sobre criação de arquivos temporários em C: https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm