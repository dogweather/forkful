---
title:                "Trabalhando com csv"
html_title:           "C: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Porque

Trabalhar com arquivos CSV (Comma Separated Values) é uma tarefa comum para muitos programadores, especialmente quando se trata de manipulação de dados. O CSV é um formato de arquivo simples e fácil de entender, o que o torna uma escolha popular para armazenar e trocar dados entre sistemas.

## Como Fazer

Para começar, é necessário incluir a biblioteca <stdio.h> e utilizar o comando "fopen" para abrir o arquivo CSV desejado. Em seguida, o comando "fscanf" pode ser utilizado para ler os dados do arquivo e armazená-los em variáveis. Por exemplo:

```C
#include <stdio.h>
 
int main()
{
    FILE *arq_csv = fopen("planilha.csv", "r"); 
    // Abre o arquivo CSV em modo de leitura
    
    char nome[20];
    int idade;
    fscanf(arq_csv, "%s, %d", nome, &idade); 
    // Lê os dados do arquivo e armazena em variáveis
    
    printf("Nome: %s, Idade: %d", nome, idade);
    // Imprime os dados lidos do arquivo 
    
    fclose(arq_csv); 
    // Fecha o arquivo
}

```

Exemplo de conteúdo no arquivo "planilha.csv":

```C
João, 25
Ana, 30
Pedro, 27
```

Exemplo de saída no console:

```
Nome: João, Idade: 25
```

## Mergulho Profundo

Além de ler os dados de um arquivo CSV, também é possível escrever informações em um novo arquivo CSV utilizando o comando "fprintf". Para isso, é necessário abrir o arquivo em modo de escrita com o parâmetro "w":

```C
#include <stdio.h>
 
int main()
{
    FILE *arq_csv = fopen("novaplanilha.csv", "w"); 
    // Abre o arquivo CSV em modo de escrita
    
    char nome[20];
    int idade;
    
    printf("Nome: ");
    scanf("%s", nome); // Lê o nome digitado pelo usuário
    
    printf("Idade: ");
    scanf("%d", &idade); // Lê a idade digitada pelo usuário
    
    fprintf(arq_csv, "%s, %d", nome, idade); 
    // Escreve os dados no novo arquivo CSV 
    
    fclose(arq_csv); 
    // Fecha o arquivo
}
```

Exemplo de saída no arquivo "novaplanilha.csv" (caso tenha sido digitado "Maria" e "35" como nome e idade, respectivamente):

```C
Maria, 35
```

## Veja Também

- [Tutorial de leitura e escrita de arquivos em C](https://www.devmedia.com.br/leitura-e-escrita-de-arquivos-em-c/24893)
- [Documentação oficial sobre a função fopen](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Guia completo sobre arquivos CSV em C](https://letslearn-c.org/csv-reading-and-writing/)