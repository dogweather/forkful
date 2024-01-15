---
title:                "Escrevendo um arquivo de texto"
html_title:           "C: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo de texto?

Criar um arquivo de texto é uma tarefa comum em programação, usada para armazenar dados de forma simples e acessível. Com o C, é possível criar e manipular arquivos de texto de forma eficiente e flexível.

## Como fazer

Para criar um arquivo de texto no C, é necessário seguir os seguintes passos:

1. Abrir um arquivo usando a função `fopen()` e especificando o modo de leitura ou escrita.
2. Escrever ou ler dados no arquivo usando as funções `fputc()` e `fgetc()`.
3. Fechar o arquivo usando a função `fclose()` para evitar perda de dados.

Um exemplo de código que cria um arquivo de texto e escreve uma string nele seria:

```C
FILE *arq = fopen("arquivo.txt", "w");
if(arq == NULL) //verifica se o arquivo foi aberto corretamente
{
    printf("Erro ao abrir o arquivo. Verifique o nome e permissões.\n");
}
else
{
    fputs("Olá mundo!", arq); //escreve a string no arquivo
    fclose(arq); //fecha o arquivo
    
    printf("Arquivo criado e modificado com sucesso!\n");
}
```

Para ler o conteúdo de um arquivo de texto, podemos usar o seguinte código:

```C
FILE *arq = fopen("arquivo.txt", "r");
if(arq == NULL)
{
    printf("Erro ao abrir o arquivo. Verifique o nome e permissões.\n");
}
else
{
    char caracter;
    while((caracter = fgetc(arq)) != EOF) //enquanto não for alcançado o final do arquivo
    {
        printf("%c", caracter); //imprime o caractere
    }
    fclose(arq); //fecha o arquivo
}
```

## Profundidade

Ao criar um arquivo de texto no C, é importante ter em mente algumas características importantes:

- Para abrir um arquivo, é necessário especificar o modo de leitura ou escrita desejado. Os modos mais comuns são: `"r"` para leitura, `"w"` para escrita (sobrescrevendo o conteúdo anterior, se existir) e `"a"` para escrita no final do arquivo.
- Ao usar a função `fputc()`, é necessário lembrar de converter o caractere em um tipo `char` usando a função `char()`.
- Para ler caracteres de um arquivo, é necessário armazená-los em uma variável do tipo `int`, pois a função `fgetc()` retorna um inteiro que representa o valor ASCII do caractere.
- É importante sempre verificar se o arquivo foi aberto corretamente antes de realizar operações de escrita ou leitura. Caso contrário, pode haver perda de dados ou falhas na execução do programa.

## Veja também

Aqui estão alguns recursos adicionais para aprimorar seus conhecimentos sobre a criação e manipulação de arquivos de texto em C:

- [Tutorialspoint - Manipulação de arquivos em C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [W3Schools - Leitura e escrita de arquivos em C](https://www.w3schools.in/c-tutorial/file-input-and-output/)
- [Site do Núcleo de Computação da UFES - Manipulação de arquivos com C](http://www.nc.ufes.br/ensino-material/desenvolvimento-de-sistemas/linguagem-c/manipulando-arquivos-em-c)