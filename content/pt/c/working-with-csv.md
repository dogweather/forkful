---
title:                "C: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

CSV (Comma-Separated Values) é um formato de arquivo amplamente utilizado para armazenar dados tabulares, como planilhas. Trabalhar com CSV é uma habilidade importante para qualquer programador de linguagem C, pois permite a manipulação de grandes quantidades de dados de forma eficiente e conveniente.

## Como fazer:

Para trabalhar com CSV em um programa C, primeiro precisamos incluir a biblioteca "stdio.h" em nosso código. Em seguida, podemos abrir um arquivo CSV usando a função `fopen()` e especificando o modo de leitura como "r". Uma vez que o arquivo esteja aberto, podemos ler os dados usando a função `fscanf()` e armazená-los em variáveis adequadas. Por fim, é importante fechar o arquivo usando a função `fclose()`.

Um exemplo prático seria o seguinte:

```
#include <stdio.h>

int main() {
    FILE *arquivo;
    char nome[30];
    int idade;
    float altura;

    arquivo = fopen("dados.csv", "r");
    if (arquivo == NULL) {
        printf("Erro ao abrir o arquivo!");
        return 1;
    }

    while(fscanf(arquivo, "%s,%d,%f", nome, &idade, &altura) != EOF) {
        printf("Nome: %s, Idade: %d, Altura: %.2f\n", nome, idade, altura);
    }

    fclose(arquivo);
    return 0;
}
```

Supondo que o arquivo "dados.csv" contenha os seguintes dados:

```
Ana,25,1.65
João,30,1.75
Maria,35,1.60
```

A saída do programa seria:

```
Nome: Ana, Idade: 25, Altura: 1.65
Nome: João, Idade: 30, Altura: 1.75
Nome: Maria, Idade: 35, Altura: 1.60
```

Note que utilizamos o formato `"Nome:%s, Idade:%d, Altura:%.2f"` na função `printf()` para imprimir os valores de forma formatada.

## Profundidade:

Além da leitura de dados, também é possível escrever em arquivos CSV usando a função `fprintf()`. É importante lembrar que o formato de leitura e escrita deve ser consistente, caso contrário os dados não serão processados corretamente.

Outro ponto importante a ser destacado é que, em alguns sistemas operacionais, os arquivos CSV utilizam ponto-e-vírgula como separador ao invés de vírgula. Portanto, é necessário estar ciente deste detalhe ao trabalhar com esses arquivos.

## Veja também:
- [Documentação da função `fscanf()`](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.4.0/com.ibm.zos.v2r4.bpxbd00/fscanf.htm)
- [Tutorial sobre leitura de arquivos CSV em C](https://www.programiz.com/c-programming/c-file-input-output)
- [Artigo sobre a diferença entre ponto-e-vírgula e vírgula nos arquivos CSV](https://www.tutlane.com/tutorial/spreadsheets/google-sheets-commas-vs-semicolons-delimiters)