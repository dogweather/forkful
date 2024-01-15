---
title:                "Lendo um arquivo de texto"
html_title:           "C: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Você pode se perguntar por que alguém se interessaria em ler um arquivo de texto em um programa de C. Bem, a resposta é simples: muitas vezes, dados são armazenados em arquivos de texto e precisamos criar programas para ler e manipular esses dados.

## Como Fazer

Para ler um arquivo de texto em um programa de C, precisamos seguir alguns passos simples.

Primeiro, precisamos abrir o arquivo utilizando a função `fopen()`. Isso é feito da seguinte forma:

```C
FILE *arquivo;
arquivo = fopen("arquivo.txt", "r");
```

A função `fopen()` recebe dois argumentos: o nome do arquivo (no exemplo acima, "arquivo.txt") e o modo de abertura, que, no caso de leitura, é "r".

Em seguida, precisamos criar uma variável para armazenar o conteúdo do arquivo em nossa memória. Neste exemplo, usaremos o tipo `char` para armazenar cada caractere do arquivo.

```C
char caractere;
```

Agora, podemos usar um laço `while` para percorrer o arquivo e imprimir seu conteúdo na tela. O laço continuará até encontrar o final do arquivo (EOF).

```C
while((caractere = fgetc(arquivo)) != EOF) {
    printf("%c", caractere);
}
```

E finalmente, não podemos esquecer de fechar o arquivo depois de terminar de lê-lo, utilizando a função `fclose()`.

```C
fclose(arquivo);
```

## Mergulho Profundo

Além da função `fgetc()`, existem outras funções úteis para ler arquivos de texto em um programa de C, como `fgets()` e `fscanf()`. Vale a pena explorar essas funções e descobrir a mais adequada para o seu programa.

Também é importante lembrar de verificar se o arquivo foi aberto corretamente, utilizando a função `ferror()`, e lidar com possíveis erros utilizando as funções `perror()` e `strerror()`. Isso garantirá que seu programa seja robusto e não apresente falhas ao lidar com arquivos de texto.

## Veja Também

- [Documentação sobre a função fopen()](https://en.cppreference.com/w/c/io/fopen)
- [Tutorial sobre leitura de arquivos em C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Exemplos de uso da função fgetc()](https://www.programiz.com/c-programming/library-function/stdio.h/fgetc)

Pronto! Agora você está pronto para ler arquivos de texto em seus programas de C. Espero que este artigo tenha sido útil e, caso queira se aprofundar ainda mais, não hesite em consultar as fontes listadas acima. Boa codificação!