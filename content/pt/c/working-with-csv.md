---
title:                "Trabalhando com CSV"
date:                  2024-01-19
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Trabalhar com CSV é lidar com dados em arquivos "Comma-Separated Values", uma forma padrão de armazenar dados tabulares. Programadores usam CSV por ser simples, amplamente suportado e fácil de importar em ferramentas de análise de dados.

## Como Fazer:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *fp = fopen("dados.csv", "r");
    if (!fp) {
        printf("Erro ao abrir o arquivo.\n");
        return 1;
    }

    char linha[1024];
    while (fgets(linha, 1024, fp)) {
        char *token = strtok(linha, ",");
        while (token) {
            printf("%s ", token);
            token = strtok(NULL, ",");
        }
        printf("\n");
    }

    fclose(fp);
    return 0;
}
```

Output de exemplo quando executado com um arquivo CSV de entrada típico:
```
id nome pontuação 
1 João 200 
2 Maria 250 
```

## Aprofundando

CSV não é padronizado; variações ocorrem em como delimitadores e quebras de linha são tratados. Alternativas como JSON ou XML são usadas quando a estrutura dos dados é mais complexa. Ao implementar em C, é fundamental cuidar da alocação de memória e do processamento de strings para evitar vulnerabilidades de segurança.

## Veja Também

- RFC 4180, que descreve o formato CSV: https://tools.ietf.org/html/rfc4180
- Tutorial sobre o uso de libcsv para C: https://sourceforge.net/projects/libcsv/
- Documentação sobre a biblioteca Standard I/O do C: https://en.cppreference.com/w/c/io
