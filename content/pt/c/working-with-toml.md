---
title:                "Trabalhando com TOML"
date:                  2024-01-26T04:19:34.313794-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
TOML é uma linguagem de serialização de dados projetada para ser fácil de ler e escrever. Programadores a utilizam para arquivos de configuração, armazenamento de dados simples e troca de dados entre linguagens devido à sua clareza e facilidade de uso para humanos.

## Como fazer:
Vamos analisar um arquivo de configuração TOML em C usando a biblioteca "tomlc99". Primeiro, instale a biblioteca. Depois, crie um `config.toml`:

```toml
title = "Exemplo TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Agora, analise-o em C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Erro: não é possível abrir o arquivo de configuração\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Erro: %s\n", errbuf);
        return 1;
    }

    printf("Título: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Nome do Proprietário: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```

Saída de exemplo:
```
Título: "Exemplo TOML"
Nome do Proprietário: "Tom Preston-Werner"
```

## Mergulho Profundo
TOML, que significa Tom's Obvious, Minimal Language (Linguagem Óbvia e Mínima de Tom), foi criada por Tom Preston-Werner em 2013. Serve como uma alternativa mais simples a formatos como XML e YAML, focando em ser mais legível e escrevível por humanos. Embora o JSON seja outra alternativa, o TOML mantém uma estrutura que é mais fácil de ser analisada visualmente por humanos, o que é uma das principais razões para sua adoção em arquivos de configuração.

Em C, trabalhar com TOML envolve escolher uma biblioteca de análise, uma vez que a linguagem não a suporta nativamente. Bibliotecas como "tomlc99" são compatíveis com C99 e fornecem uma API para decodificar textos TOML. Ao considerar o desempenho, o tratamento adequado de erros e a gestão de memória são cruciais, pois C não possui coleta de lixo embutida.

## Veja Também:
1. Especificação TOML: [https://toml.io/pt/](https://toml.io/pt/)
2. Repositório GitHub tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Comparação de Formatos de Serialização de Dados: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
