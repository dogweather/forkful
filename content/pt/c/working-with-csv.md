---
title:                "Trabalhando com CSV"
html_title:           "C: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-csv.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Trabalhar com CSV significa lidar com arquivos no formato Comma-Separated Values, que consiste em dados separados por vírgulas. Programadores frequentemente trabalham com CSVs porque é uma maneira eficiente de armazenar e manipular grandes conjuntos de dados.

## Como fazer:
``` C
#include<stdio.h>

int main() 
{
  // Abrindo um arquivo CSV existente
  FILE * csv = fopen("meu_arquivo.csv", "r");
  
  // Lendo os dados e imprimindo-os na tela
  char linha[100];
  while(fgets(linha, sizeof(linha), csv)) 
  {
    printf("%s", linha);
  }
  
  // Fechando o arquivo
  fclose(csv);
  
  return 0;
}
```

Sample output:
```
coluna1,coluna2,coluna3
1,2,3
4,5,6
7,8,9
``` 

## Profundando:
CSVs surgiram na década de 1970 como uma forma de compartilhar dados entre diferentes programas. Em vez de utilizar um formato proprietário, os desenvolvedores optaram por usar um padrão simples e fácil de interpretar. Existem outras formas de armazenar dados tabulares, como JSON e XML, mas o CSV continua sendo popular por ser leve e fácil de trabalhar com.

## Veja também:
- [Aprenda a trabalhar com CSV em C](https://www.programiz.com/c-programming/c-file-input-output)