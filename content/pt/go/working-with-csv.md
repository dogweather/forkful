---
title:                "Trabalhando com csv"
html_title:           "Go: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/working-with-csv.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com CSV (Comma-Separated Values) é uma forma de armazenar e manipular dados em formato de tabela, onde os valores são separados por vírgulas. Programadores costumam utilizar esse formato para importar e exportar dados entre diferentes sistemas ou para realizar cálculos e análises.

## Como fazer:

Exemplo de código em Go para ler um arquivo CSV e imprimir seus valores:

```
func main() {
    arquivo, erro := os.Open("dados.csv")
    if erro != nil {
        fmt.Println("Erro ao abrir o arquivo:", erro)
    }
    defer arquivo.Close()

    csvReader := csv.NewReader(arquivo)
    registros, erro := csvReader.ReadAll()
    if erro != nil {
        fmt.Println("Erro ao ler o arquivo CSV:", erro)
    }

    for _, registro := range registros {
        fmt.Println(registro)
    }
}
```

Saída de exemplo, supondo que o arquivo CSV contenha os valores "Nome" e "Idade" em suas colunas:

```
[Nome Idade]
[João 25]
[Maria 30]
[Carlos 40]
```

## Mergulho Profundo:

- Contexto histórico: O CSV foi criado nos anos 70 para facilitar o compartilhamento de dados entre sistemas diferentes.
- Alternativas: Alguns formatos de dados populares incluem JSON e XML, que são mais estruturados e possuem recursos adicionais, como a capacidade de armazenar dados em forma de árvore.
- Detalhes de implementação: A biblioteca padrão do Go possui o pacote "encoding/csv" para lidar com a leitura e o processamento de arquivos CSV. É importante lembrar de tratar os possíveis erros durante a leitura e fechar o arquivo após seu uso.

## Ver também:

- Documentação oficial do pacote "encoding/csv": https://golang.org/pkg/encoding/csv/
- Artigo sobre os diferentes formatos de dados: https://www.freecodecamp.org/news/the-different-data-formats-of-robust-data-structures/